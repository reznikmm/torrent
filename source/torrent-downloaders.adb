--  Copyright (c) 2019-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with AWS.Response;
with AWS.Client;
with AWS.Messages;

with League.IRIs;

with Torrent.Contexts;
with Torrent.Logs;
with Torrent.Trackers;

package body Torrent.Downloaders is

   use type Ada.Streams.Stream_Element_Offset;

   procedure Check_Stored_Pieces (Self : in out Downloader'Class);

   procedure Send_Tracker_Request
     (Self  : in out Downloader'Class;
      Event : Torrent.Trackers.Announcement_Kind);

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
      URL : constant League.IRIs.IRI := Trackers.Event_URL
        (Tracker    => Self.Meta.Announce,
         Info_Hash  => Self.Meta.Info_Hash,
         Peer_Id    => Self.Peer_Id,
         Port       => Self.Port,
         Uploaded   => Self.Uploaded,
         Downloaded => Self.Downloaded,
         Left       => Self.Left,
         Event      => Event);

      Reply : constant AWS.Response.Data :=
        AWS.Client.Get
          (URL.To_Universal_String.To_UTF_8_String,
           Follow_Redirection => True);
   begin
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

         return;
      elsif Self.Left = 0 or Event not in Torrent.Trackers.Started then
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
      end;
   end Send_Tracker_Request;

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
