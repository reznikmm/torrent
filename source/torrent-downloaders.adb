--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with GNAT.SHA1;
with GNAT.Sockets;

with AWS.Response;
with AWS.Client;
with AWS.Messages;

with League.IRIs;

with Torrent.Handshakes;

package body Torrent.Downloaders is

   use type Ada.Streams.Stream_Element_Offset;

   package Connection_Queue_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Torrent.Connections.Connection_Access);

   package Connection_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Connection_Queue_Interfaces);

   type Connection_Access_Array is
     array (Positive range <>) of Torrent.Connections.Connection_Access;

   procedure Best_Connections
     (Self   : Downloader'Class;
      Result : out Connection_Access_Array);

   procedure Check_Stored_Pieces (Self : in out Downloader'Class);

   type Downloader_Access is access all Downloader'Class
     with Storage_Size => 0;

   package Downloader_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => SHA1,
      Element_Type => Downloader_Access,
      "<"          => Ada.Streams."<",
      "="          => "=");

   function Find_Download (Hash : SHA1) return Downloader_Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Torrent.Connections.Connection'Class,
      Torrent.Connections.Connection_Access);

   task type Session is
      entry Seed
        (Downloader : not null Downloader_Access;
         Value      : not null Torrent.Connections.Connection_Access);

      entry Stop_Seeding;
      entry Stop;
   end Session;

   task Manager is
      entry New_Download (Value : not null Downloader_Access);
      entry Connected (Value : not null Torrent.Connections.Connection_Access);
      entry Complete;
   end Manager;

   task Initiator is
      entry Connect
        (Downloader : not null Downloader_Access;
         Value      : not null Torrent.Connections.Connection_Access);
      entry Stop;
   end Initiator;

   Recycle : Connection_Queues.Queue;
   --  Connections, returned by Manager back to Initiator to recconnect or
   --  destroy.

   Port : constant := 33411;

   Downloader_List : Downloader_Maps.Map;

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

   -------------------
   -- Find_Download --
   -------------------

   function Find_Download (Hash : SHA1) return Downloader_Access is
      Cursor : constant Downloader_Maps.Cursor := Downloader_List.Find (Hash);
   begin
      if Downloader_Maps.Has_Element (Cursor) then
         return Downloader_Maps.Element (Cursor);
      else
         return null;
      end if;
   end Find_Download;

   ---------------
   -- Initiator --
   ---------------

   task body Initiator is
      type Socket_Connection is record
         Socket : GNAT.Sockets.Socket_Type;
         Item   : Torrent.Connections.Connection_Access;
         Job    : Downloader_Access;
      end record;

      package Socket_Connection_Vectors is new Ada.Containers.Vectors
        (Positive, Socket_Connection);

      type Planned_Connection is record
         Time : Ada.Calendar.Time;
         Item : Torrent.Connections.Connection_Access;
         Job  : Downloader_Access;
      end record;

      function Less (Left, Right : Planned_Connection) return Boolean;

      package Planned_Connection_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Planned_Connection,
         "<"          => Less);

      type Accepted_Connection is record
         Socket  : GNAT.Sockets.Socket_Type;
         Address : GNAT.Sockets.Sock_Addr_Type;
         Data    : Torrent.Handshakes.Handshake_Image;
         Last    : Ada.Streams.Stream_Element_Count;
         Done    : Boolean;
         Session : Torrent.Connections.Connection_Access;
      end record;

      package Accepted_Connection_Vectors is new Ada.Containers.Vectors
        (Positive, Accepted_Connection);

      procedure Accept_Connection
        (Server   : GNAT.Sockets.Socket_Type;
         Accepted : in out Accepted_Connection_Vectors.Vector);

      procedure Read_Handshake (Item : in out Accepted_Connection);

      function Hash (Item : Torrent.Connections.Connection_Access)
        return Ada.Containers.Hash_Type;

      package Connection_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type        => Torrent.Connections.Connection_Access,
         Hash                => Hash,
         Equivalent_Elements => Torrent.Connections."=",
         "="                 => Torrent.Connections."=");

      function "+"
        (Value : Torrent.Connections.Connection_Access)
            return System.Storage_Elements.Integer_Address;

      ---------
      -- "+" --
      ---------

      function "+"
        (Value : Torrent.Connections.Connection_Access)
            return System.Storage_Elements.Integer_Address
      is
         package Conv is new System.Address_To_Access_Conversions
           (Object => Torrent.Connections.Connection'Class);
      begin
         return System.Storage_Elements.To_Integer
           (Conv.To_Address (Conv.Object_Pointer (Value)));
      end "+";

      -----------------------
      -- Accept_Connection --
      -----------------------

      procedure Accept_Connection
        (Server   : GNAT.Sockets.Socket_Type;
         Accepted : in out Accepted_Connection_Vectors.Vector)
      is
         Address : GNAT.Sockets.Sock_Addr_Type;
         Socket  : GNAT.Sockets.Socket_Type;
      begin
         GNAT.Sockets.Accept_Socket (Server, Socket, Address);
         Ada.Text_IO.Put_Line ("Accepted: " & GNAT.Sockets.Image (Address));

         GNAT.Sockets.Set_Socket_Option
           (Socket => Socket,
            Level  => GNAT.Sockets.Socket_Level,
            Option => (GNAT.Sockets.Receive_Timeout, 0.0));

         Accepted.Append
           ((Socket,
            Last    => 0,
            Done    => False,
            Address => Address,
            others  => <>));
      end Accept_Connection;

      ----------
      -- Hash --
      ----------

      function Hash (Item : Torrent.Connections.Connection_Access)
        return Ada.Containers.Hash_Type is
      begin
         return Ada.Containers.Hash_Type'Mod (+Item);
      end Hash;

      ----------
      -- Less --
      ----------

      function Less (Left, Right : Planned_Connection) return Boolean is
         use type Ada.Calendar.Time;
         use type System.Storage_Elements.Integer_Address;

      begin
         return Left.Time < Right.Time
           or else (Left.Time = Right.Time and +Left.Item < +Right.Item);
      end Less;

      --------------------
      -- Read_Handshake --
      --------------------

      procedure Read_Handshake (Item : in out Accepted_Connection) is
         use Torrent.Handshakes;
         use type Ada.Streams.Stream_Element;
         Last : Ada.Streams.Stream_Element_Count;
         Job  : Downloader_Access;
      begin
         GNAT.Sockets.Receive_Socket
           (Item.Socket,
            Item.Data (Item.Last + 1 .. Item.Data'Last),
            Last);

         if Last <= Item.Last then
            Item.Done := True;  --  Connection closed
         elsif Last = Item.Data'Last then
            declare
               Value : constant Handshake_Type := -Item.Data;
            begin
               Job := Find_Download (Value.Info_Hash);
               Item.Done := True;

               if Value.Length = Header'Length
                 and then Value.Head = Header
                 and then Job /= null
               then
                  Item.Session := new Torrent.Connections.Connection
                    (Job.Meta,
                     Job.Storage'Unchecked_Access,
                     Job.Meta.Piece_Count);

                  Item.Session.Initialize
                    (My_Id    => Job.Peer_Id,
                     Peer     => Item.Address,
                     Listener => Job.Tracked'Unchecked_Access);

                  Item.Session.Do_Handshake
                    (Item.Socket,
                     Job.Completed (1 .. Job.Last_Completed),
                     Inbound => True);
               end if;
            end;
         else
            Item.Last := Last;
         end if;
      exception
         when E : GNAT.Sockets.Socket_Error =>

            if GNAT.Sockets.Resolve_Exception (E) not in
              GNAT.Sockets.Resource_Temporarily_Unavailable
            then
               Item.Done := True;
            end if;
      end Read_Handshake;

      use type Ada.Calendar.Time;

      Address  : constant GNAT.Sockets.Sock_Addr_Type :=
        (GNAT.Sockets.Family_Inet, GNAT.Sockets.Any_Inet_Addr, Port);
      Server   : GNAT.Sockets.Socket_Type;
      Plan     : Planned_Connection_Sets.Set;
      Work     : Socket_Connection_Vectors.Vector;
      Accepted : Accepted_Connection_Vectors.Vector;
      Inbound  : Connection_Sets.Set;
      Selector : aliased GNAT.Sockets.Selector_Type;
      Time     : Ada.Calendar.Time := Ada.Calendar.Clock + 20.0;
      W_Set    : GNAT.Sockets.Socket_Set_Type;
      R_Set    : GNAT.Sockets.Socket_Set_Type;
      Session  : Torrent.Connections.Connection_Access;
   begin
      GNAT.Sockets.Create_Selector (Selector);
      GNAT.Sockets.Create_Socket (Server);
      GNAT.Sockets.Bind_Socket (Server, Address);
      GNAT.Sockets.Listen_Socket (Server);

      loop
         loop
            select
               Recycle.Dequeue (Session);

               if Inbound.Contains (Session) then
                  Inbound.Delete (Session);
                  Free (Session);
               else
                  Time := Ada.Calendar.Clock;
                  Plan.Insert
                    ((Time + 300.0,
                     Session,
                     Find_Download (Session.Meta.Info_Hash)));
               end if;
            else
               exit;
            end select;
         end loop;

         select
            accept Stop;
            exit;
         or
            accept Connect
              (Downloader : not null Downloader_Access;
               Value      : not null Torrent.Connections.Connection_Access)
            do
               Time := Ada.Calendar.Clock;
               Plan.Insert ((Time, Value, Downloader));
            end Connect;
         or
            delay until Time;
         end select;

         while not Plan.Is_Empty loop
            declare
               Ignore : GNAT.Sockets.Selector_Status;
               First  : constant Planned_Connection := Plan.First_Element;
               Socket : GNAT.Sockets.Socket_Type;
            begin
               exit when First.Time > Time;

               Ada.Text_IO.Put_Line
                 ("Connecting: " & GNAT.Sockets.Image (First.Item.Peer));

               GNAT.Sockets.Create_Socket (Socket);
               GNAT.Sockets.Connect_Socket
                 (Socket   => Socket,
                  Server   => First.Item.Peer,
                  Timeout  => 0.0,
                  Status   => Ignore);
               Work.Append ((Socket, First.Item, First.Job));
               Plan.Delete_First;
            end;
         end loop;

         GNAT.Sockets.Empty (W_Set);
         GNAT.Sockets.Empty (R_Set);
         GNAT.Sockets.Set (R_Set, Server);

         for J of Accepted loop
            GNAT.Sockets.Set (R_Set, J.Socket);
         end loop;

         for J of Work loop
            GNAT.Sockets.Set (W_Set, J.Socket);
         end loop;

         if GNAT.Sockets.Is_Empty (W_Set)
           and GNAT.Sockets.Is_Empty (R_Set)
         then
            Time := Ada.Calendar.Clock + 20.0;
         else
            declare
               Status : GNAT.Sockets.Selector_Status;
            begin
               GNAT.Sockets.Check_Selector
                 (Selector,
                  R_Set,
                  W_Set,
                  Status,
                  Timeout => 0.2);

               if Status in GNAT.Sockets.Completed then
                  declare
                     J : Positive := 1;
                  begin
                     if GNAT.Sockets.Is_Set (R_Set, Server) then
                        Accept_Connection (Server, Accepted);
                     end if;

                     while J <= Accepted.Last_Index loop
                        if GNAT.Sockets.Is_Set
                          (R_Set, Accepted (J).Socket)
                        then
                           Read_Handshake (Accepted (J));

                           if Accepted (J).Done then
                              if Accepted (J).Session in null then
                                 GNAT.Sockets.Close_Socket
                                   (Accepted (J).Socket);
                              else
                                 Inbound.Insert (Accepted (J).Session);
                                 Manager.Connected (Accepted (J).Session);
                              end if;

                              Accepted.Swap (J, Accepted.Last_Index);
                              Accepted.Delete_Last;
                           else
                              J := J + 1;
                           end if;
                        else
                           J := J + 1;
                        end if;
                     end loop;

                     J := 1;

                     while J <= Work.Last_Index loop
                        if GNAT.Sockets.Is_Set (W_Set, Work (J).Socket) then
                           Work (J).Item.Do_Handshake
                             (Work (J).Socket,
                              Work (J).Job.Completed
                              (1 .. Work (J).Job.Last_Completed),
                              Inbound => False);

                           if Work (J).Item.Connected then
                              Manager.Connected (Work (J).Item);
                           else
                              Plan.Insert
                                ((Time + 300.0, Work (J).Item, Work (J).Job));
                           end if;

                           Work.Swap (J, Work.Last_Index);
                           Work.Delete_Last;
                        else
                           J := J + 1;
                        end if;
                     end loop;
                  end;
               end if;

               Time := Ada.Calendar.Clock + 1.0;
            end;
         end if;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Initiator: " & Ada.Exceptions.Exception_Information (E));
   end Initiator;

   -------------
   -- Manager --
   -------------

   task body Manager is
      use type Torrent.Connections.Connection_Access;
      Job : Downloader_Access;

      Sessions : array (1 .. 1) of Session;
      Slowdown : Duration := 0.5;
   begin
      accept New_Download (Value : not null Downloader_Access) do
         Job := Value;
      end New_Download;

      loop
         select
            accept Connected
              (Value : in not null Torrent.Connections.Connection_Access)
            do
               Job.Chocked.Append (Value);
               Slowdown := 0.0;
            end Connected;
         or
            accept Complete;
            exit;
         else
            delay Slowdown;
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
            declare
               J : Positive := 1;
               Conn : Torrent.Connections.Connection_Access;
            begin
               while J <= Job.Chocked.Last_Index loop
                  Conn := Job.Chocked (J);

                  if not Conn.Connected then
                     Recycle.Enqueue (Conn);
                     Job.Chocked.Delete (J);
                  elsif not (for some X of List => X = Conn) then
                     Conn.Serve (Job.Completed (1 .. Job.Last_Completed), 1.0);
                     J := J + 1;
                  else
                     J := J + 1;
                  end if;
               end loop;
            end;

            for J in List'Range loop
               if List (J) /= null then
                  Sessions (J).Stop_Seeding;
               end if;
            end loop;
         end;
      end loop;

      for J in Sessions'Range loop
         Sessions (J).Stop;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
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
         select
            accept Stop;
            exit;

         or
            accept Seed
              (Downloader : not null Downloader_Access;
               Value      : not null Torrent.Connections.Connection_Access)
            do
               Job := Downloader;
               Conn := Value;
            end Seed;

         end select;

         if Conn.Intrested then
            Conn.Set_Choked (False);
            Conn.Serve (Job.Completed (1 .. Job.Last_Completed), Seed_Time);
            Conn.Set_Choked (True);
         end if;

         accept Stop_Seeding;
      end loop;
   end Session;

   -------------------------
   -- Check_Stored_Pieces --
   -------------------------

   procedure Check_Stored_Pieces (Self : in out Downloader'Class) is
   begin
      for J in 1 .. Self.Piece_Count loop
         if Connections.Is_Valid_Piece (Self.Meta, Self.Storage, J) then
            Self.Tracked.Piece_Completed (J, True);
         end if;
      end loop;
   end Check_Stored_Pieces;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self : aliased in out Downloader'Class;
      Path : League.String_Vectors.Universal_String_Vector)
   is
      procedure Set_Peer_Id (Value : out SHA1);
      procedure Download;

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

      --------------
      -- Download --
      --------------

      procedure Download is
         URL : League.IRIs.IRI := Trackers.Event_URL
           (Tracker    => Self.Meta.Announce,
            Info_Hash  => Self.Meta.Info_Hash,
            Peer_Id    => Self.Peer_Id,
            Port       => Self.Port,
            Uploaded   => Self.Uploaded,
            Downloaded => Self.Downloaded,
            Left       => Self.Left,
            Event      => Trackers.Started);

         Reply : constant AWS.Response.Data :=
           AWS.Client.Get
             (URL.To_Universal_String.To_UTF_8_String,
              Follow_Redirection => True);
      begin
         if AWS.Response.Status_Code (Reply) not in AWS.Messages.Success then
            Ada.Text_IO.Put_Line
              ("Tracker request failed:"
               & AWS.Messages.Status_Code'Image
                 (AWS.Response.Status_Code (Reply)));
            Ada.Text_IO.Put_Line (URL.To_Universal_String.To_UTF_8_String);
            return;
         end if;

         Self.Tracker_Response := new Torrent.Trackers.Response'
           (Trackers.Parse (AWS.Response.Message_Body (Reply)));

         Ada.Text_IO.Put_Line
           ("Peer_Count:" & (Self.Tracker_Response.Peer_Count'Img));

         for J in 1 .. Self.Tracker_Response.Peer_Count loop
            declare
               TR : Torrent.Trackers.Response
                 renames Self.Tracker_Response.all;

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

               Initiator.Connect (Self'Unchecked_Access, Connection);
            end;
         end loop;

         URL := Trackers.Event_URL
           (Tracker    => Self.Meta.Announce,
            Info_Hash  => Self.Meta.Info_Hash,
            Peer_Id    => Self.Peer_Id,
            Port       => Self.Port,
            Uploaded   => Self.Uploaded,
            Downloaded => Self.Downloaded,
            Left       => Self.Left,
            Event      => Trackers.Completed);

         declare
            Ignore : constant AWS.Response.Data :=
              AWS.Client.Get
                (URL.To_Universal_String.To_UTF_8_String,
                 Follow_Redirection => True);
         begin
            null;  --  Just notify tracker
         end;
      end Download;

   begin
      Downloader_List.Insert (Self.Meta.Info_Hash, Self'Unchecked_Access);

      Set_Peer_Id (Self.Peer_Id);
      Self.Path := Path;
      Self.Port := Port;
      Self.Chocked.Clear;
      Self.Left := 0;
      Self.Downloaded := 0;
      Self.Uploaded := 0;
      Self.Last_Completed := 0;
      Self.Storage.Initialize (Path.Join ('/'));

      Self.Tracked.Initialize
        (Self.Meta.Piece_Length, Self.Meta.Last_Piece_Length);

      for J in 1 .. Self.Meta.File_Count loop
         Self.Left := Self.Left + Self.Meta.File_Length (J);
      end loop;

      Self.Check_Stored_Pieces;
      Ada.Text_IO.Put_Line ("Left bytes:" & (Self.Left'Img));

      Manager.New_Download (Self'Unchecked_Access);

      if Self.Left > 0  then
         Download;
      else
         declare
            URL : constant League.IRIs.IRI := Trackers.Event_URL
              (Tracker    => Self.Meta.Announce,
               Info_Hash  => Self.Meta.Info_Hash,
               Peer_Id    => Self.Peer_Id,
               Port       => Self.Port,
               Uploaded   => Self.Uploaded,
               Downloaded => Self.Downloaded,
               Left       => Self.Left,
               Event      => Trackers.Regular);

            Ignore : constant AWS.Response.Data :=
              AWS.Client.Get
                (URL.To_Universal_String.To_UTF_8_String,
                 Follow_Redirection => True);
         begin
            null;  --  Just notify tracker
         end;
         delay 3600.0;  --  Seed file for some time
      end if;

      Manager.Complete;
      Initiator.Stop;
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
         Ok    : Boolean) is
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
         end if;
      end Piece_Completed;

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
