--  Copyright (c) 2019-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces;

with AWS.Response;
with AWS.Client;
with AWS.Messages;

with League.IRIs;
with League.String_Vectors;

with Torrent.Contexts;
with Torrent.Logs;
with Torrent.Trackers;
with Torrent.UDP_Tracker_Protocol;

package body Torrent.Downloaders is

   use type Ada.Streams.Stream_Element_Offset;

   procedure Check_Stored_Pieces (Self : in out Downloader'Class);

   procedure Send_Tracker_Request
     (Self  : in out Downloader'Class;
      Event : Torrent.Trackers.Announcement_Kind);

   procedure Send_Tracker_Request
     (Self     : in out Downloader'Class;
      Event    : Torrent.Trackers.Announcement_Kind;
      Announce : League.IRIs.IRI;
      Success  : out Boolean);

   procedure Send_UDP_Tracker_Request
     (Self     : in out Downloader'Class;
      Event    : Torrent.Trackers.Announcement_Kind;
      Announce : League.Strings.Universal_String;
      Success  : out Boolean);

   -------------------------
   -- Check_Stored_Pieces --
   -------------------------

   procedure Check_Stored_Pieces (Self : in out Downloader'Class) is
   begin
      if not Self.Storage.Is_Empty_Storage then
         for J in 1 .. Self.Piece_Count loop
            if Connections.Is_Valid_Piece (Self.Meta, Self.Storage, J) then
               Self.Tracked.Piece_Completed (J, True);
            end if;
         end loop;
      end if;
   end Check_Stored_Pieces;

   ---------------
   -- Completed --
   ---------------

   function Completed (Self : Downloader'Class)
     return Torrent.Connections.Piece_Index_Array is
   begin
      return Self.Completed (1 .. Self.Last_Completed);
   end Completed;

   --------------------
   -- Create_Session --
   --------------------

   function Create_Session
     (Self    : in out Downloader'Class;
      Address : GNAT.Sockets.Sock_Addr_Type)
      return Torrent.Connections.Connection_Access
   is
      Result : constant Torrent.Connections.Connection_Access :=
        new Torrent.Connections.Connection
          (Self.Meta,
           Self'Unchecked_Access,
           Self.Storage'Unchecked_Access,
           Self.Meta.Piece_Count);
   begin
      Result.Initialize
        (My_Id    => Self.Peer_Id,
         Peer     => Address,
         Listener => Self.Tracked'Unchecked_Access);

      return Result;
   end Create_Session;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Downloader'Class;
      Peer : SHA1;
      Path : League.Strings.Universal_String) is
   begin
      Self.Peer_Id := Peer;
      Self.Port := Self.Context.Port;
      Self.Left := 0;
      Self.Downloaded := 0;
      Self.Uploaded := 0;
      Self.Last_Completed := 0;
      Self.Storage.Initialize (Path);

      Self.Tracked.Initialize
        (Self.Meta.Piece_Length, Self.Meta.Last_Piece_Length);

      for J in 1 .. Self.Meta.File_Count loop
         Self.Left := Self.Left + Self.Meta.File_Length (J);
      end loop;

      Self.Check_Stored_Pieces;

      pragma Debug
        (Torrent.Logs.Enabled,
         Torrent.Logs.Print ("Left bytes:" & (Self.Left'Img)));
   end Initialize;

   ----------------
   -- Is_Leacher --
   ----------------

   function Is_Leacher (Self : Downloader'Class) return Boolean is
   begin
      return Self.Left > 0;
   end Is_Leacher;

   --------------------------
   -- Send_Tracker_Request --
   --------------------------

   procedure Send_Tracker_Request
     (Self  : in out Downloader'Class;
      Event : Torrent.Trackers.Announcement_Kind)
   is
      Success : Boolean := False;
      List    : constant Torrent.Metainfo_Files.String_Vector_Array :=
        Self.Meta.Announce_List;
   begin
      if List'Length = 0 then
         Self.Send_Tracker_Request
           (Event, Self.Meta.Announce, Success);
      else
         for Item of List loop
            for J in 1 .. Item.Length loop
               declare
                  URL : constant League.Strings.Universal_String := Item (J);
               begin
                  if URL.Starts_With ("udp://") then
                     Self.Send_UDP_Tracker_Request
                       (Event,
                        URL.Tail_From (7),
                        Success);
                  else
                     Self.Send_Tracker_Request
                       (Event,
                        League.IRIs.From_Universal_String (URL),
                        Success);
                  end if;

                  if Success then
                     return;
                  end if;
               exception
                  when Constraint_Error =>
                     null;
               end;
            end loop;
         end loop;
      end if;
   end Send_Tracker_Request;

   --------------------------
   -- Send_Tracker_Request --
   --------------------------

   procedure Send_Tracker_Request
     (Self     : in out Downloader'Class;
      Event    : Torrent.Trackers.Announcement_Kind;
      Announce : League.IRIs.IRI;
      Success  : out Boolean)
   is
      URL : constant League.IRIs.IRI := Trackers.Event_URL
        (Tracker    => Announce,
         Info_Hash  => Self.Meta.Info_Hash,
         Peer_Id    => Self.Peer_Id,
         Port       => Self.Port,
         Uploaded   => Self.Uploaded,
         Downloaded => Self.Downloaded,
         Left       => Self.Left,
         Event      => Event);

      Reply : AWS.Response.Data;
   begin
      if Announce.Get_Scheme.To_Wide_Wide_String not in "http" | "https" then
         Success := False;
         return;
      else
         begin
            Reply :=
              AWS.Client.Get
                (URL.To_Universal_String.To_UTF_8_String,
                 Follow_Redirection => True);
         exception
            when others =>
               Success := False;
               return;
         end;
      end if;

      pragma Debug
        (Torrent.Logs.Enabled,
         Torrent.Logs.Print
           ("Tracker URL:" & URL.To_Universal_String.To_UTF_8_String));

      if AWS.Response.Status_Code (Reply) not in AWS.Messages.Success then
         pragma Debug
           (Torrent.Logs.Enabled,
            Torrent.Logs.Print
              ("Tracker request failed:"
               & AWS.Messages.Status_Code'Image
                 (AWS.Response.Status_Code (Reply))));

         pragma Debug
           (Torrent.Logs.Enabled,
            Torrent.Logs.Print (URL.To_Universal_String.To_UTF_8_String));

         Success := False;

         return;
      elsif Self.Left = 0 or Event not in Torrent.Trackers.Started then
         Success := True;

         return;
      end if;

      declare
         TR : constant Torrent.Trackers.Response :=
           Trackers.Parse (AWS.Response.Message_Body (Reply));
      begin
         pragma Debug
           (Torrent.Logs.Enabled,
            Torrent.Logs.Print
              ("Peer_Count:" & (TR.Peer_Count'Img)));

         for J in 1 .. TR.Peer_Count loop
            declare
               Address    : constant GNAT.Sockets.Sock_Addr_Type :=
                 (Family => GNAT.Sockets.Family_Inet,
                  Addr   => GNAT.Sockets.Inet_Addr
                    (TR.Peer_Address (J).To_UTF_8_String),
                  Port   => GNAT.Sockets.Port_Type (TR.Peer_Port (J)));

            begin
               pragma Debug
                 (Torrent.Logs.Enabled,
                  Torrent.Logs.Print
                    ((J'Img) & " /" & (TR.Peer_Count'Img)
                     & " Initialize connection to "
                     & TR.Peer_Address (J).To_UTF_8_String));

               Self.Context.Connect (Self'Unchecked_Access, Address);
            end;
         end loop;

         Success := True;
      end;
   end Send_Tracker_Request;

   ------------------------------
   -- Send_UDP_Tracker_Request --
   ------------------------------

   procedure Send_UDP_Tracker_Request
     (Self     : in out Downloader'Class;
      Event    : Torrent.Trackers.Announcement_Kind;
      Announce : League.Strings.Universal_String;
      Success  : out Boolean)
   is
      use Torrent.UDP_Tracker_Protocol;

      Colon  : constant Natural := Announce.Last_Index (':');
      Server : GNAT.Sockets.Socket_Type;
      Addr   : GNAT.Sockets.Sock_Addr_Type;

      Connect_Req : constant Connect_Request :=
        (Action => 0, Transaction_Id => 1234, Protocol_Id => <>);

      Announce_Req : Announce_Request;

      Buffer  : Ada.Streams.Stream_Element_Array (1 .. 1600);
      Last    : Ada.Streams.Stream_Element_Count;
      Connect : Interfaces.Unsigned_64;
   begin
      Success := False;

      if Colon = 0 then
         return;
      else
         begin
            Addr.Addr := GNAT.Sockets.Inet_Addr
              (Announce.Head_To (Colon - 1).To_UTF_8_String);
            Addr.Port := GNAT.Sockets.Port_Type'Value
              (Announce.Tail_From (Colon + 1).To_UTF_8_String);
         exception
            when Constraint_Error =>
               return;
         end;
      end if;

      GNAT.Sockets.Create_Socket
        (Server, GNAT.Sockets.Family_Inet, GNAT.Sockets.Socket_Datagram);

      GNAT.Sockets.Set_Socket_Option
        (Server,
         GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Reuse_Address, True));

      GNAT.Sockets.Set_Socket_Option
        (Server,
         GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Receive_Timeout,
          Timeout => 15.0));

      GNAT.Sockets.Bind_Socket
        (Server,
         (GNAT.Sockets.Family_Inet,
          GNAT.Sockets.Any_Inet_Addr,
          GNAT.Sockets.Port_Type (Self.Port)));

      begin
         GNAT.Sockets.Send_Socket
           (Server,
            Cast (Connect_Req),
            Last,
            To     => Addr);
         pragma Assert (Last = Raw_Connect_Request'Last);

         GNAT.Sockets.Receive_Socket (Server, Buffer, Last, Addr);
      exception
         when GNAT.Sockets.Socket_Error =>
            return;
      end;

      if Last >= 16 then
         declare
            use type Interfaces.Unsigned_32;
            Response : constant Connect_Response :=
              Cast (Buffer (1 .. 16));
         begin
            if Response.Action /= Connect_Req.Action
              or else Response.Transaction_Id /= Connect_Req.Transaction_Id
            then
               return;
            else
               Connect := Response.Connection_Id;
            end if;
         end;
      else
         return;
      end if;

      Announce_Req :=
        (Connection_Id  => Connect,
         Action         => 1,  --  announce
         Transaction_Id => 4321,
         Info_Hash      => Self.Meta.Info_Hash,
         Peer_Id        => Self.Peer_Id,
         Downloaded     => Interfaces.Unsigned_64 (Self.Downloaded),
         Left           => Interfaces.Unsigned_64 (Self.Left),
         Uploaded       => Interfaces.Unsigned_64 (Self.Uploaded),
         Event          => Cast_Event (Event),
         IP_Address     => <>,
         Key            => 0,
         Num_Want       => <>,
         Port           => Interfaces.Unsigned_16 (Self.Port));

      begin
         GNAT.Sockets.Send_Socket
           (Server,
            Cast (Announce_Req),
            Last,
            To => Addr);
         pragma Assert (Last = Raw_Announce_Request'Last);

         GNAT.Sockets.Receive_Socket (Server, Buffer, Last, Addr);
      exception
         when GNAT.Sockets.Socket_Error =>
            return;
      end;

      if Last >= Raw_Announce_Response'Last then
         declare
            use type Interfaces.Unsigned_32;
            Response : constant Announce_Response :=
              Cast (Buffer (1 .. Raw_Announce_Response'Last));
         begin
            if Response.Action /= Announce_Req.Action
              or else Response.Transaction_Id /= Announce_Req.Transaction_Id
            then
               return;
            end if;
         end;
      else
         return;
      end if;

      for J in 0 .. (Last - Raw_Announce_Response'Last) / 6 loop
         declare
            Peer    : constant Peer_Address := Cast
              (Buffer (21 + J * 6 .. 26 + J * 6));
            X1      : constant String := Peer.IP_Address (1)'Image;
            X2      : String := Peer.IP_Address (2)'Image;
            X3      : String := Peer.IP_Address (3)'Image;
            X4      : String := Peer.IP_Address (4)'Image;

            Address : GNAT.Sockets.Sock_Addr_Type;

         begin
            X2 (1) := '.';
            X3 (1) := '.';
            X4 (1) := '.';

            Address :=
              (Family => GNAT.Sockets.Family_Inet,
               Addr   => GNAT.Sockets.Inet_Addr
                 (X1 (2 .. X1'Last) & X2 & X3 & X4),
               Port   => GNAT.Sockets.Port_Type (Peer.Port));

            pragma Debug
              (Torrent.Logs.Enabled,
               Torrent.Logs.Print
                 ((J'Img)
                  & " Initialize connection to "
                  & GNAT.Sockets.Image (Address)));

            Self.Context.Connect (Self'Unchecked_Access, Address);
         end;
      end loop;

      Success := True;
   end Send_UDP_Tracker_Request;

   -----------
   -- Start --
   -----------

   procedure Start (Self : aliased in out Downloader'Class) is
   begin
      Self.Send_Tracker_Request (Torrent.Trackers.Started);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Downloader'Class) is
   begin
      Self.Send_Tracker_Request (Torrent.Trackers.Stopped);
   end Stop;

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Downloader'Class) is
   begin
      Self.Send_Tracker_Request (Torrent.Trackers.Regular);
   end Update;

   --------------------
   -- Tracked_Pieces --
   --------------------

   protected body Tracked_Pieces is

      function Get_Piece_Size (Piece : Piece_Index) return Piece_Offset;

      --------------------
      -- Get_Piece_Size --
      --------------------

      function Get_Piece_Size (Piece : Piece_Index) return Piece_Offset is
      begin
         if Piece = Piece_Count then
            return Last_Piece_Size;
         else
            return Piece_Size;
         end if;
      end Get_Piece_Size;

      procedure Initialize
        (Piece_Length      : Piece_Offset;
         Last_Piece_Length : Piece_Offset)
      is
      begin
         Piece_Size := Piece_Length;
         Last_Piece_Size := Last_Piece_Length;
      end Initialize;

      --------------------
      -- Interval_Saved --
      --------------------

      procedure Interval_Saved
        (Piece : Piece_Index;
         Value : Torrent.Connections.Interval;
         Last  : out Boolean)
      is
         use type Torrent.Connections.Interval;
         Cursor : constant Piece_State_Maps.Cursor := Finished.Find (Piece);
      begin
         Downloader.Downloaded := Downloader.Downloaded +
           Value.To - Value.From + 1;

         if Piece_State_Maps.Has_Element (Cursor) then
            Torrent.Connections.Insert (Finished (Cursor), Value);

            Last := Finished (Cursor).Last_Index = 1
              and then Finished (Cursor).Last_Element =
                (0, Get_Piece_Size (Piece) - 1);

            return;
         end if;

         Finished.Insert
           (Piece,
            Torrent.Connections.Interval_Vectors.To_Vector
              (Value, Length => 1));

         Last := Value = (0, Get_Piece_Size (Piece) - 1);
      end Interval_Saved;

      ---------------------
      -- Piece_Completed --
      ---------------------

      procedure Piece_Completed
        (Piece : Piece_Index;
         Ok    : Boolean)
      is
         Is_Leecher : constant Boolean := Downloader.Left > 0;
      begin
         Our_Map (Piece) := Ok;

         if Finished.Contains (Piece) then
            Finished.Delete (Piece);
         end if;

         if Unfinished.Contains (Piece) then
            Unfinished.Delete (Piece);
         end if;

         if Ok then
            Downloader.Left := Downloader.Left - Get_Piece_Size (Piece);
            Downloader.Completed (Downloader.Last_Completed + 1) := Piece;
            Downloader.Last_Completed := Downloader.Last_Completed + 1;

            if Is_Leecher and Downloader.Left = 0 then
               --  FIXME It's bad to do blocking operation from protected type.
               pragma Debug
                 (Torrent.Logs.Enabled,
                  Torrent.Logs.Print ("Download complete:"));
               Downloader.Send_Tracker_Request (Torrent.Trackers.Completed);
            end if;
         end if;
      end Piece_Completed;


      -------------------
      -- Interval_Sent --
      -------------------

      procedure Interval_Sent (Size : Piece_Offset) is
      begin
         Downloader.Uploaded := Downloader.Uploaded + Size;
      end Interval_Sent;

      -----------------------
      -- Reserve_Intervals --
      -----------------------

      procedure Reserve_Intervals
        (Map        : Boolean_Array;
         Value      : out Torrent.Connections.Piece_State)
      is

         procedure Get_Intervals
           (Item : in out Torrent.Connections.Interval_Vectors.Vector);

         -------------------
         -- Get_Intervals --
         -------------------

         procedure Get_Intervals
           (Item : in out Torrent.Connections.Interval_Vectors.Vector) is
         begin
            while not Item.Is_Empty loop
               declare
                  Last : Torrent.Connections.Interval :=
                    Item.Last_Element;
               begin
                  while Last.To - Last.From + 1 > Max_Interval_Size loop
                     Value.Intervals.Append
                       ((Last.From, Last.From + Max_Interval_Size - 1));

                     Last.From := Last.From + Max_Interval_Size;

                     if Value.Intervals.Last_Index >= 16 then
                        Item (Item.Last_Index) := Last;
                        return;
                     end if;
                  end loop;

                  Value.Intervals.Append (Last);
                  Item.Delete_Last;
               end;
            end loop;
         end Get_Intervals;
      begin
         Value.Intervals.Clear;

         for Item in Unfinished.Iterate loop
            if not Unfinished (Item).Is_Empty
              and then Map (Piece_State_Maps.Key (Item))
            then
               Value.Piece := Piece_State_Maps.Key (Item);
               Get_Intervals (Unfinished (Item));
               return;
            end if;
         end loop;

         for J in Map'Range loop
            if not Our_Map (J)
              and then Map (J)
              and then not Unfinished.Contains (J)
            then
               Unfinished.Insert
                 (J, Torrent.Connections.Interval_Vectors.To_Vector
                    ((0, Get_Piece_Size (J) - 1), Length => 1));
               Reserve_Intervals (Map, Value);
               return;
            end if;
         end loop;
      end Reserve_Intervals;

      ----------------------
      -- We_Are_Intrested --
      ----------------------

      function We_Are_Intrested
        (Map : Boolean_Array) return Boolean is
      begin
         return (Map and not Our_Map) /= (1 .. Piece_Count => False);
      end We_Are_Intrested;

      -------------------------
      -- Unreserve_Intervals --
      -------------------------

      procedure Unreserve_Intervals
        (Value : Torrent.Connections.Piece_Interval_Array) is
      begin
         for J of Value loop
            if Unfinished.Contains (J.Piece) then
               Unfinished (J.Piece).Append (J.Span);
            else
               Unfinished.Insert
                 (J.Piece,
                  Torrent.Connections.Interval_Vectors.To_Vector (J.Span, 1));
            end if;
         end loop;
      end Unreserve_Intervals;

   end Tracked_Pieces;

end Torrent.Downloaders;
