--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Strings.Hash;
with Interfaces;

with GNAT.SHA1;

package body Torrent.Downloaders is

   Header : constant String := "BitTorrent protocol";

   -----------------
   -- Get_Request --
   -----------------

   procedure Get_Request (Self : in out Downloader; Value : out Request) is

      function Handshake
        return League.Stream_Element_Vectors.Stream_Element_Vector;

      ---------------
      -- Handshake --
      ---------------

      function Handshake
        return League.Stream_Element_Vectors.Stream_Element_Vector
      is
         Result : League.Stream_Element_Vectors.Stream_Element_Vector;
         Zero   : constant Ada.Streams.Stream_Element_Array := (1 .. 8 => 0);
      begin
         Result.Append (Header'Length);

         for Char of Header loop
            Result.Append (Character'Pos (Char));
         end loop;

         Result.Append (Zero);
         Result.Append (Self.Meta.Info_Hash);
         Result.Append (Self.Peer_Id);

         return Result;
      end Handshake;

   begin
      if Self.Tracker_Response = null then
         declare
            URL : constant League.IRIs.IRI := Trackers.Event_URL
              (Tracker    => Self.Meta.Announce,
               Info_Hash  => Self.Meta.Info_Hash,
               Peer_Id    => Self.Peer_Id,
               Port       => Self.Port,
               Uploaded   => Self.Uploaded,
               Downloaded => Self.Downloaded,
               Left       => Self.Left,
               Event      => Trackers.Started);
         begin
            Value := (Tracker_Request, URL);
            return;
         end;
      end if;

      declare
         TR : Torrent.Trackers.Response renames Self.Tracker_Response.all;
      begin
         while Natural (Self.Servers.Length) < TR.Peer_Count loop
            declare
               J : constant Positive := Natural (Self.Servers.Length) + 1;

               Address : constant GNAT.Sockets.Sock_Addr_Type :=
                 (Family => GNAT.Sockets.Family_Inet,
                  Addr   => GNAT.Sockets.Addresses
                    (GNAT.Sockets.Get_Host_By_Name
                         (TR.Peer_Address (J).To_UTF_8_String)),
                  Port   => GNAT.Sockets.Port_Type (TR.Peer_Port (J)));

               Server : constant Server_State :=
                 (Peer         => Address,
                  Handshake    => True,
                  Closed       => False,
                  We_Choked    => True,
                  He_Intrested => False,
                  First        => 0,  --  Not assigned yet
                  Unparsed     => <>);
            begin
               Self.Servers.Insert (Server.Peer, Server);
               Value := (Peer_Request, Address, Handshake);
            end;
         end loop;
      end;
   end Get_Request;

   ---------------
   -- Handshake --
   ---------------

   procedure Handshake
     (Self   : in out Downloader;
      Server : in out Server_State;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Array;

      Msg  : constant Ada.Streams.Stream_Element_Array :=
        Value.To_Stream_Element_Array;
      Last : constant Ada.Streams.Stream_Element_Count :=
        1 + Header'Length + 8 + 20 + 20;
   begin
      if Msg (1) /= Header'Length or Msg'Length < Last then
         Server.Closed := True;
      else
         for J in Header'Range loop
            if Msg (Ada.Streams.Stream_Element_Count (J + 1))
              /= Character'Pos (Header (J))
            then
               Server.Closed := True;
               exit;
            end if;
         end loop;

         if Self.Meta.Info_Hash /= Msg (Last - 19 .. Last) then
            Server.Closed := True;
         end if;
      end if;

      if Server.Closed then
         return;  --  Bad handshake
      end if;

      Server.Unparsed.Clear;

      Server.First := Self.Last + 1;
      Self.Last := Self.Last + Self.Meta.Piece_Count;

      if Last < Msg'Last then
         Server.Unparsed.Append (Msg (Last + 1 .. Msg'Last));
         Self.Message
           (Server, League.Stream_Element_Vectors.Empty_Stream_Element_Vector);
      end if;
   end Handshake;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : GNAT.Sockets.Sock_Addr_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (GNAT.Sockets.Image (Value));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Downloader'Class;
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
         Value (1) := 33; Value := (2 .. Value'Last => 33);
      end Set_Peer_Id;

      Last_Piece_Size : Ada.Streams.Stream_Element_Count;
   begin
      Set_Peer_Id (Self.Peer_Id);
      Self.Path := Path;
      Self.Port := Port;
      Self.Pieces.Clear;
      Self.Left := 0;
      Self.Downloaded := 0;
      Self.Uploaded := 0;
      Self.Piece_Map :=
        new Boolean_Array'(1 .. 5 * Self.Meta.Piece_Count => False);
      Self.Last := Self.Meta.Piece_Count;

      for J in 1 .. Self.Meta.Piece_Count loop
         Self.Pieces.Append ((Intervals => Interval_Vectors.Empty_Vector));
      end loop;

      for J in 1 .. Self.Meta.File_Count loop
         Self.Left := Self.Left + Self.Meta.File_Length (J);
      end loop;

      Last_Piece_Size := Self.Left mod Self.Meta.Piece_Length;

      if Last_Piece_Size = 0 then
         Last_Piece_Size := Self.Meta.Piece_Length;
      end if;

      Self.Last_Piece_Size := Positive (Last_Piece_Size);
   end Initialize;

   -------------
   -- Message --
   -------------

   procedure Message
     (Self   : in out Downloader;
      Server : in out Server_State;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;

      subtype X is Ada.Streams.Stream_Element_Offset;

      Msg : constant Ada.Streams.Stream_Element_Array :=
        Server.Unparsed.To_Stream_Element_Array
          & Value.To_Stream_Element_Array;

      Last : Ada.Streams.Stream_Element_Count := Msg'First - 1;
      Length : Ada.Streams.Stream_Element_Count;
   begin
      if Msg'Length < 4 then
         Server.Unparsed.Append (Msg);
         return;
      end if;

      loop
         Length :=
           ((X (Msg (Last + 1)) * 256
            + X (Msg (Last + 2))) * 256
            + X (Msg (Last + 3))) * 256
           + X (Msg (Last + 4));

         if Msg'Last - Last < 4 + Length then
            Server.Unparsed.Clear;
            Server.Unparsed.Append (Msg (Last .. Msg'Last));
            return;
         else
            Last := Last + 4;
            Self.Message (Server, Msg (Last + 1 .. Last + Length));
            Last := Last + Length;
         end if;
      end loop;
   end Message;

   -------------
   -- Message --
   -------------

   procedure Message
     (Self   : in out Downloader;
      Server : in out Server_State;
      Value  : Ada.Streams.Stream_Element_Array)
   is
      function Get_Int
        (From : Ada.Streams.Stream_Element_Count := 0) return Natural;

      function Piece_Size (Index : Positive) return Positive;

      procedure Save_Piece
        (Index  : Positive;
         Offset : Natural;
         Value  : Ada.Streams.Stream_Element_Array);

      procedure Append
        (List  : in out Interval_Vectors.Vector;
         Value : Interval);

      -------------
      -- Get_Int --
      -------------

      function Get_Int
        (From : Ada.Streams.Stream_Element_Count := 0) return Natural
      is
         use type Ada.Streams.Stream_Element_Offset;
         subtype X is Natural;
      begin
         return
           ((X (Value (Value'First + From)) * 256
            + X (Value (Value'First + From + 1))) * 256
            + X (Value (Value'First + From + 2))) * 256
            + X (Value (Value'First + From + 3));
      end Get_Int;

      ------------
      -- Append --
      ------------

      procedure Append
        (List  : in out Interval_Vectors.Vector;
         Value : Interval)
      is
         M, N : Natural := 0;
         Next : Interval := Value;
      begin
         loop
            for J in 1 .. List.Last_Index loop
               if List (J).To + 1 = Next.From then
                  M := J;
                  exit;
               elsif List (J).From - 1 = Next.To then
                  N := J;
                  exit;
               end if;
            end loop;

            if M > 0 then
               Next := (List (M).From, Next.To);
               List.Swap (M, List.Last_Index);
               List.Delete_Last;
            elsif N > 0 then
               Next := (Next.From, List (N).To);
               List.Swap (N, List.Last_Index);
               List.Delete_Last;
            else
               List.Append (Next);
               exit;
            end if;
         end loop;
      end Append;

      ----------------
      -- Piece_Size --
      ----------------

      function Piece_Size (Index : Positive) return Positive is
      begin
         if Index in 1 .. Self.Meta.Piece_Count - 1 then
            return Natural (Self.Meta.Piece_Length);
         else
            return Self.Last_Piece_Size;
         end if;
      end Piece_Size;

      ----------------
      -- Save_Piece --
      ----------------

      procedure Save_Piece
        (Index  : Positive;
         Offset : Natural;
         Value  : Ada.Streams.Stream_Element_Array)
      is
         Piece : Piece_State renames Self.Pieces (Index);
         From  : constant Natural := Offset;
         To    : constant Positive := From + Value'Length;
      begin
         Append (Piece.Intervals, (From, To));
         if Piece.Intervals.Last_Index = 1
           and then Piece.Intervals.Last_Element = (1, Piece_Size (Index))
         then
            --  TODO check SHA1
            Self.Piece_Map (Index) := True;
         end if;
      end Save_Piece;

   begin
      case Value (Value'First) is
         when 0 =>  --  choke
            Server.We_Choked := True;
         when 1 =>  -- unchoke
            Server.We_Choked := False;
         when 2 =>  -- interested
            Server.He_Intrested := True;
         when 3 =>  -- not interested
            Server.He_Intrested := False;

         when 4 =>  -- have
            declare
               Index : constant Natural := Get_Int;
            begin
               if Index in 1 .. Self.Meta.Piece_Count then

                  Self.Piece_Map (Server.First + Index - 1) := True;
               end if;
            end;

         when 5 => -- bitfield
            declare
               Index : Positive := 1;
            begin
               Each_Byte :
               for X of Value loop
                  declare
                     use type Interfaces.Unsigned_8;
                     Byte : Interfaces.Unsigned_8 :=
                       Interfaces.Unsigned_8 (X);
                  begin
                     for J in 1 .. 8 loop
                        if (Byte and 16#80#) /= 0 then
                           Self.Piece_Map (Server.First + Index - 1) := True;
                        end if;

                        Byte := Interfaces.Shift_Left (Byte, 1);
                        Index := Index + 1;

                        exit Each_Byte when Index > Self.Meta.Piece_Count;
                     end loop;
                  end;
               end loop Each_Byte;
            end;

         when 6 => -- request
            declare
               Index  : constant Positive := Get_Int;
               Offset : constant Natural := Get_Int (4);
               Length : constant Positive := Get_Int (8);
            begin
               if Index in 1 .. Self.Meta.Piece_Count then
                  Self.Commands.Append
                    ((New_Request, Index, Offset, Length));
               end if;
            end;

         when 7 => -- piece
            declare
               use type Ada.Streams.Stream_Element_Offset;
               Index  : constant Positive := Get_Int;
               Offset : constant Natural := Get_Int (4);
            begin
               if Index in 1 .. Self.Meta.Piece_Count
                 and then not Self.Piece_Map (Index)
                 and then Value'Length > 8
               then
                  Save_Piece
                    (Index, Offset, Value (Value'First + 8 .. Value'Last));
               end if;
            end;

         when 8 => -- cancel
            declare
               Index  : constant Positive := Get_Int;
               Offset : constant Natural := Get_Int (4);
               Length : constant Positive := Get_Int (8);
               Cursor : Command_Vectors.Cursor := Self.Commands.Find
                 ((New_Request, Index, Offset, Length));
            begin
               if Command_Vectors.Has_Element (Cursor) then
                  Self.Commands.Delete (Cursor);
               end if;
            end;
         when others =>  --  unknown
            null;
      end case;
   end Message;

   ------------------
   -- New_Response --
   ------------------

   procedure New_Response (Self : in out Downloader; Value : Response) is
   begin
      case Value.Kind is
         when Tracker_Request =>
            if not Value.Success or Self.Tracker_Response /= null then
               return;
            end if;

            Self.Tracker_Response := new Torrent.Trackers.Response'
              (Trackers.Parse (Value.Message.To_Stream_Element_Array));

            Self.Servers.Reserve_Capacity
              (Ada.Containers.Count_Type (Self.Tracker_Response.Peer_Count));

         when Peer_Request =>
            if not Self.Servers.Contains (Value.Peer) then
               return;  --  TODO client processing
            end if;

            declare
               Server : Server_State renames Self.Servers (Value.Peer);
            begin
               if Server.Handshake then
                  Self.Handshake (Server, Value.Message);
               else
                  Self.Message (Server, Value.Message);
               end if;

               if Value.Closed then
                  Server.Closed := True;
               end if;
            end;
      end case;
   end New_Response;

end Torrent.Downloaders;
