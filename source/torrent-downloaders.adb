--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.SHA1;
with GNAT.Sockets;

with AWS.Response;
with AWS.Client;
with AWS.Messages;

with League.IRIs;

package body Torrent.Downloaders is

   type Connection_Access_Array is
     array (Positive range <>) of Torrent.Connections.Connection_Access;

   procedure Best_Connections
     (Self   : Downloader'Class;
      Result : out Connection_Access_Array);

   type Downloader_Access is access all Downloader'Class
     with Storage_Size => 0;

   task type Session is
      entry Seed
        (Downloader : not null Downloader_Access;
         Value      : not null Torrent.Connections.Connection_Access);

      entry Stop_Seeding;
   end Session;

   task Manager is
      entry New_Download (Value : not null Downloader_Access);
      entry Connected (Value : not null Torrent.Connections.Connection_Access);
      entry Complete;
   end Manager;

   task Initiator is
      entry Connect (Value : not null Torrent.Connections.Connection_Access);
   end Initiator;

   ----------------------
   -- Best_Connections --
   ----------------------

   procedure Best_Connections
     (Self   : Downloader'Class;
      Result : out Connection_Access_Array)
   is
      Index : Positive := Result'First;
   begin
      Result := (Result'Range => null);

      for X of Self.Chocked loop
         if X.Intrested then
            Result (Index) := X;

            exit when Index = Result'Last;

            Index := Index + 1;
         end if;
      end loop;
   end Best_Connections;

   ---------------
   -- Initiator --
   ---------------

   task body Initiator is
      Next :  Torrent.Connections.Connection_Access;
   begin
      loop
         accept Connect
           (Value : not null Torrent.Connections.Connection_Access)
         do
            Next := Value;
         end Connect;

         begin
            Next.Serve ((1 .. 0 => <>), 0.1);

            if Next.Connected then
               Manager.Connected (Next);
            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 ("Initiator: " & Ada.Exceptions.Exception_Information (E));
         end;
      end loop;
   end Initiator;

   -------------
   -- Manager --
   -------------

   task body Manager is
      use type Ada.Streams.Stream_Element_Offset;
      use type Torrent.Connections.Connection_Access;
      Job : Downloader_Access;

      Sessions : array (1 .. 1) of Session;
   begin
      accept New_Download (Value : not null Downloader_Access) do
         Job := Value;
      end New_Download;

      while Job.Left > 0 loop
         select
            accept Connected
              (Value : in not null Torrent.Connections.Connection_Access)
            do
               Job.Chocked.Append (Value);
            end Connected;
         else
            delay 0.2;
         end select;

         declare
            List : Connection_Access_Array (Sessions'Range);
         begin
            Job.Best_Connections (List);

            for J in List'Range loop
               if List (J) /= null then
                  Sessions (J).Seed (Job, List (J));
               end if;
            end loop;

            --  process chocked connections.
            for J of Job.Chocked loop
               if J.Connected and then not (for some X of List => X = J) then
                  J.Serve (Job.Completed (1 .. Job.Last_Completed), 0.1);
               end if;
            end loop;

            for J in List'Range loop
               if List (J) /= null then
                  Sessions (J).Stop_Seeding;
               end if;
            end loop;
         end;
      end loop;

      accept Complete;
   end Manager;

   -------------
   -- Session --
   -------------

   task body Session is
      Seed_Time : constant Duration := 10.0;

      Job  : Downloader_Access;
      Conn : Torrent.Connections.Connection_Access;
   begin
      loop
         accept Seed
           (Downloader : not null Downloader_Access;
            Value      : not null Torrent.Connections.Connection_Access)
         do
            Job := Downloader;
            Conn := Value;
         end Seed;

         if Conn not in null
           and then Conn.Intrested
         then
            Conn.Set_Choked (False);
            Conn.Serve (Job.Completed (1 .. Job.Last_Completed), Seed_Time);
            Conn.Set_Choked (True);
         end if;

         accept Stop_Seeding;
      end loop;
   end Session;
   -----------
   -- Start --
   -----------

   procedure Start
     (Self : aliased in out Downloader'Class;
      Port : Positive;
      Path : League.String_Vectors.Universal_String_Vector)
   is
      use type Ada.Streams.Stream_Element_Count;

      procedure Set_Peer_Id (Value : out SHA1);

      -----------------
      -- Set_Peer_Id --
      -----------------

      procedure Set_Peer_Id (Value : out SHA1) is
         Now : constant String := Ada.Calendar.Formatting.Image
           (Ada.Calendar.Clock);
         Context : GNAT.SHA1.Context;
      begin
         GNAT.SHA1.Update (Context, Path.Join ("/").To_UTF_8_String);
         GNAT.SHA1.Update (Context, Self.Meta.Info_Hash);
         GNAT.SHA1.Update (Context, Now);
         GNAT.SHA1.Update (Context, GNAT.Sockets.Host_Name);

         Value := GNAT.SHA1.Digest (Context);
         --  For test purpose
         Value := (1 .. Value'Last => 33);
      end Set_Peer_Id;

      URL : League.IRIs.IRI;
   begin
      Set_Peer_Id (Self.Peer_Id);
      Self.Path := Path;
      Self.Port := Port;
      Self.Chocked.Clear;
      Self.Left := 0;
      Self.Downloaded := 0;
      Self.Uploaded := 0;
      Self.Last_Completed := 0;
      Self.Storage.Initialize (Path.Join ('/'));

      for J in 1 .. Self.Meta.File_Count loop
         Self.Left := Self.Left + Self.Meta.File_Length (J);
      end loop;

      Self.Tracked.Initialize
        (Self.Meta.Piece_Length, Self.Meta.Last_Piece_Length);

      URL := Trackers.Event_URL
        (Tracker    => Self.Meta.Announce,
         Info_Hash  => Self.Meta.Info_Hash,
         Peer_Id    => Self.Peer_Id,
         Port       => Self.Port,
         Uploaded   => Self.Uploaded,
         Downloaded => Self.Downloaded,
         Left       => Self.Left,
         Event      => Trackers.Started);

      declare
         Reply : constant AWS.Response.Data :=
           AWS.Client.Get
             (URL.To_Universal_String.To_UTF_8_String,
              Follow_Redirection => True);
      begin
         if AWS.Response.Status_Code (Reply) not in AWS.Messages.Success then
            return;
         end if;

         Self.Tracker_Response := new Torrent.Trackers.Response'
           (Trackers.Parse (AWS.Response.Message_Body (Reply)));
      end;

      Manager.New_Download (Self'Unchecked_Access);

      for J in 1 .. Self.Tracker_Response.Peer_Count loop
         declare
            TR : Torrent.Trackers.Response renames Self.Tracker_Response.all;

            Connection : constant Torrent.Connections.Connection_Access :=
              new Torrent.Connections.Connection
                (Self.Meta,
                 Self.Storage'Unchecked_Access,
                 Self.Meta.Piece_Count);

            Address    : constant GNAT.Sockets.Sock_Addr_Type :=
              (Family => GNAT.Sockets.Family_Inet,
               Addr   => GNAT.Sockets.Inet_Addr
                 (TR.Peer_Address (J).To_UTF_8_String),
               Port   => GNAT.Sockets.Port_Type (TR.Peer_Port (J)));

         begin
            Connection.Initialize
              (Self.Peer_Id,
               Address,
               Self.Tracked'Unchecked_Access);

            Initiator.Connect (Connection);
         end;
      end loop;

      Manager.Complete;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Start;

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
         use type Piece_Offset;
         Cursor : constant Piece_State_Maps.Cursor := Finished.Find (Piece);
      begin
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
         Ok    : Boolean) is
      begin
         Ada.Text_IO.Put_Line
           ("Piece_Completed" & (Piece'Img) & " " & (Ok'Img));

         Our_Map (Piece) := Ok;

         Finished.Delete (Piece);
         Unfinished.Delete (Piece);

         if Ok then
            Downloader.Completed (Downloader.Last_Completed + 1) := Piece;
            Downloader.Last_Completed := Downloader.Last_Completed + 1;
         end if;
      end Piece_Completed;

      -----------------------
      -- Reserve_Intervals --
      -----------------------

      procedure Reserve_Intervals
        (Map        : Boolean_Array;
         Value      : out Torrent.Connections.Piece_State)
      is
         use type Ada.Streams.Stream_Element_Offset;
         Item_Size : constant := 16 * 1024;

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
                  while Last.To - Last.From + 1 > Item_Size loop
                     Value.Intervals.Append
                       ((Last.From, Last.From + Item_Size - 1));

                     Last.From := Last.From + Item_Size;

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
