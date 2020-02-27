--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Torrent.Contexts;
with Torrent.Handshakes;
with Torrent.Logs;

package body Torrent.Initiators is

   procedure Free is new Ada.Unchecked_Deallocation
     (Torrent.Connections.Connection'Class,
      Torrent.Connections.Connection_Access);

   ---------------
   -- Initiator --
   ---------------

   task body Initiator is
      use type Ada.Streams.Stream_Element_Offset;

      type Socket_Connection is record
         Socket : GNAT.Sockets.Socket_Type;
         Addr   : GNAT.Sockets.Sock_Addr_Type;
         Job    : Torrent.Downloaders.Downloader_Access;
      end record;

      package Socket_Connection_Vectors is new Ada.Containers.Vectors
        (Positive, Socket_Connection);

      type Planned_Connection is record
         Time : Ada.Calendar.Time;
         Addr : GNAT.Sockets.Sock_Addr_Type;
         Job  : Torrent.Downloaders.Downloader_Access;
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

         pragma Debug
           (Torrent.Logs.Enabled,
            Torrent.Logs.Print ("Accepted: " & GNAT.Sockets.Image (Address)));

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
         function "+" (V : GNAT.Sockets.Sock_Addr_Type) return String
           renames GNAT.Sockets.Image;

      begin
         return Left.Time < Right.Time
           or else (Left.Time = Right.Time and +Left.Addr < +Right.Addr);
      end Less;

      --------------------
      -- Read_Handshake --
      --------------------

      procedure Read_Handshake (Item : in out Accepted_Connection) is
         use Torrent.Handshakes;
         use type Ada.Streams.Stream_Element;
         Last : Ada.Streams.Stream_Element_Count;
         Job  : Torrent.Downloaders.Downloader_Access;
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
               Job := Context.Find_Download (Value.Info_Hash);
               Item.Done := True;

               if Value.Length = Header'Length
                 and then Value.Head = Header
                 and then Job not in null
               then
                  Item.Session := Job.Create_Session (Item.Address);

                  Item.Session.Do_Handshake
                    (Item.Socket, Job.Completed, Inbound => True);
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
        (GNAT.Sockets.Family_Inet, GNAT.Sockets.Any_Inet_Addr,
         GNAT.Sockets.Port_Type (Port));
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

      Top_Loop :
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
                     Session.Peer,
                     Torrent.Downloaders.Downloader_Access
                       (Session.Downloader)));
               end if;
            else
               exit;
            end select;
         end loop;

         loop
            select
               accept Stop;
               exit Top_Loop;
            or
               accept Connect
                 (Downloader : not null Torrent.Downloaders.Downloader_Access;
                  Address    : GNAT.Sockets.Sock_Addr_Type)
               do
                  Time := Ada.Calendar.Clock;
                  Plan.Insert ((Time, Address, Downloader));
               end Connect;
            or
               delay until Time;
               exit;
            end select;
         end loop;

         while not Plan.Is_Empty loop
            declare
               Ignore : GNAT.Sockets.Selector_Status;
               First  : constant Planned_Connection := Plan.First_Element;
               Socket : GNAT.Sockets.Socket_Type;
            begin
               exit when First.Time > Time;

               if First.Job.Is_Leacher then
                  pragma Debug
                    (Torrent.Logs.Enabled,
                     Torrent.Logs.Print
                       ("Connecting: " & GNAT.Sockets.Image (First.Addr)));

                  GNAT.Sockets.Create_Socket (Socket);
                  GNAT.Sockets.Connect_Socket
                    (Socket   => Socket,
                     Server   => First.Addr,
                     Timeout  => 0.0,
                     Status   => Ignore);
                  Work.Append ((Socket, First.Addr, First.Job));
               end if;

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
                                 Context.Connected
                                   (Accepted (J).Session);
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
                           declare
                              Conn : Torrent.Connections.Connection_Access :=
                                Work (J).Job.Create_Session (Work (J).Addr);
                           begin
                              Conn.Do_Handshake
                                (Work (J).Socket,
                                 Work (J).Job.Completed,
                                 Inbound => False);

                              if Conn.Connected then
                                 Context.Connected (Conn);
                              else
                                 Free (Conn);
                                 Plan.Insert
                                   ((Time + 300.0,
                                    Work (J).Addr,
                                    Work (J).Job));
                              end if;

                              Work.Swap (J, Work.Last_Index);
                              Work.Delete_Last;
                           end;
                        else
                           J := J + 1;
                        end if;
                     end loop;
                  end;
               end if;

               Time := Ada.Calendar.Clock + 1.0;
            end;
         end if;
      end loop Top_Loop;
   exception
      when E : others =>
         pragma Debug
           (Torrent.Logs.Enabled,
            Torrent.Logs.Print
              ("Initiator: " & Ada.Exceptions.Exception_Information (E)));

         Ada.Text_IO.Put_Line
           ("Initiator: " & Ada.Exceptions.Exception_Information (E));
   end Initiator;

end Torrent.Initiators;
