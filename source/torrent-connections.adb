--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;
with Ada.Exceptions;

package body Torrent.Connections is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Array;
   use type Ada.Streams.Stream_Element_Offset;

   Header : constant String := "BitTorrent protocol";

   subtype Handshake_Image is Ada.Streams.Stream_Element_Array
     (1 .. 1 + Header'Length + 8 + 2 * SHA1'Length);

   type Handshake_Type is record
      Length    : Ada.Streams.Stream_Element := Header'Length;
      Head      : String (Header'Range) := Header;
      Zeros     : Ada.Streams.Stream_Element_Array (1 .. 8) := (others => 0);
      Info_Hash : SHA1;
      Peer_Id   : SHA1;
   end record
     with Pack, Size => 8 * (1 + Header'Length + 8 + 2 * SHA1'Length);

   function Get_Handshake (Self : Connection'Class) return Handshake_Image;

   procedure Send_Message
     (Self : Connection'Class;
      Data : Ada.Streams.Stream_Element_Array);

   -------------------
   -- Get_Handshake --
   -------------------

   function Get_Handshake (Self : Connection'Class) return Handshake_Image is
      function "+" is new Ada.Unchecked_Conversion
        (Handshake_Type, Handshake_Image);

      Result : Handshake_Type;

   begin
      Result.Info_Hash := Self.Meta.Info_Hash;
      Result.Peer_Id := Self.My_Peer_Id;

      return +Result;
   end Get_Handshake;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Connection'Class;
      My_Id    : SHA1;
      Peer     : GNAT.Sockets.Sock_Addr_Type;
      Listener : Connection_State_Listener_Access)
   is
   begin
      Self.Peer := Peer;
      Self.Sent_Handshake := False;
      Self.Got_Handshake := False;
      Self.Closed := False;
      Self.We_Choked := True;
      Self.He_Choked := True;
      Self.Choked_Sent := True;
      Self.We_Intrested := False;
      Self.He_Intrested := False;
      Self.My_Peer_Id := My_Id;
      Self.Last_Request := 0;
      Self.Listener := Listener;
      Self.Current_Piece := (0, Intervals => <>);
      Self.Piece_Map := (others => False);
   end Initialize;

   ---------------
   -- Connected --
   ---------------

   function Connected (Self : Connection'Class) return Boolean is
   begin
      return not Self.Closed;
   end Connected;

   ---------------
   -- Intrested --
   ---------------

   function Intrested (Self : Connection'Class) return Boolean is
   begin
      return not Self.Closed and Self.He_Intrested;
   end Intrested;

   ----------------
   -- Set_Choked --
   ----------------

   procedure Set_Choked (Self : in out Connection'Class; Value : Boolean) is
   begin
      if Self.He_Choked /= Value then
         Self.He_Choked := Value;
         Self.Choked_Sent := False;
      end if;
   end Set_Choked;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Self : Connection'Class;
      Data : Ada.Streams.Stream_Element_Array)
   is
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket
        (Socket => Self.Socket,
         Item   => Data,
         Last   => Last);

      Ada.Text_IO.Put_Line
        ("Send "
         & GNAT.Sockets.Image (Self.Peer)
         & (Data (Data'First + 4)'Img));

      pragma Assert (Last = Data'Last);
   end Send_Message;

   -----------
   -- Serve --
   -----------

   procedure Serve
     (Self : in out Connection'Class;
      Time : Duration)
   is
      use type Ada.Calendar.Time;

      procedure Connect_And_Send_Handshake;
      procedure Check_Intrested;
      procedure Send_Requests;
      function Get_Handshake
        (Data : Ada.Streams.Stream_Element_Array) return Boolean;

      function Get_Length
        (Data : Ada.Streams.Stream_Element_Array)
         return Ada.Streams.Stream_Element_Offset;

      procedure Read_Messages
        (Data : in out Ada.Streams.Stream_Element_Array;
         Last : out Ada.Streams.Stream_Element_Count);

      procedure On_Message (Data : Ada.Streams.Stream_Element_Array);

      procedure Save_Piece
        (Index  : Positive;
         Offset : Natural;
         Data   : Ada.Streams.Stream_Element_Array);

      ---------------------
      -- Check_Intrested --
      ---------------------

      procedure Check_Intrested is
      begin
         if not Self.We_Intrested
           and then Self.Listener.We_Are_Intrested (Self.Piece_Map)
         then
            Self.Send_Message ((00, 00, 00, 01, 02));  --  interested
            Self.We_Intrested := True;

--           and then Self.Piece_Map and not Self.My_Pieces  FIXME

         end if;
      end Check_Intrested;

      --------------------------------
      -- Connect_And_Send_Handshake --
      --------------------------------

      procedure Connect_And_Send_Handshake is
         Last   : Ada.Streams.Stream_Element_Count;
         Status : GNAT.Sockets.Selector_Status;
      begin
         Ada.Text_IO.Put_Line ("Connecting " & GNAT.Sockets.Image (Self.Peer));
         GNAT.Sockets.Create_Socket (Self.Socket);

         GNAT.Sockets.Connect_Socket
           (Socket => Self.Socket,
            Server => Self.Peer,
            Timeout => 2.0,
            Status => Status);

         if Status not in GNAT.Sockets.Completed then
            Ada.Text_IO.Put_Line ("Failed: " & GNAT.Sockets.Image (Self.Peer));
            Self.Closed := True;
            return;
         end if;

         Ada.Text_IO.Put_Line
           ("Connected to: " & GNAT.Sockets.Image (Self.Peer));

         GNAT.Sockets.Send_Socket
           (Socket => Self.Socket,
            Item   => Self.Get_Handshake,
            Last   => Last);

         pragma Assert (Last = Handshake_Image'Last);

         GNAT.Sockets.Set_Socket_Option
           (Socket => Self.Socket,
            Level  => GNAT.Sockets.Socket_Level,
            Option => (GNAT.Sockets.Receive_Timeout, 1.0));

         Self.Sent_Handshake := True;
      end Connect_And_Send_Handshake;

      -------------------
      -- Get_Handshake --
      -------------------

      function Get_Handshake
        (Data : Ada.Streams.Stream_Element_Array) return Boolean
      is
         function "+" is new Ada.Unchecked_Conversion
           (Handshake_Image, Handshake_Type);

         HS : constant Handshake_Type := +Data (1 .. Handshake_Image'Length);
      begin
         if HS.Length = Header'Length
           and then HS.Head = Header
           and then HS.Info_Hash = Self.Meta.Info_Hash
         then
            Self.Got_Handshake := True;
            Self.Unparsed.Clear;
            Self.Unparsed.Append
              (Data (Handshake_Image'Length + 1 .. Data'Last));

            return True;
         else
            return False;
         end if;
      end Get_Handshake;

      ----------------
      -- Get_Length --
      ----------------

      function Get_Length
        (Data : Ada.Streams.Stream_Element_Array)
         return Ada.Streams.Stream_Element_Offset
      is
         subtype X is Ada.Streams.Stream_Element_Offset;
      begin
         return ((X (Data (Data'First)) * 256
            + X (Data (Data'First + 1))) * 256
            + X (Data (Data'First + 2))) * 256
            + X (Data (Data'First + 3));
      end Get_Length;

      ----------------
      -- On_Message --
      ----------------

      procedure On_Message (Data : Ada.Streams.Stream_Element_Array) is
         function Get_Int
           (From : Ada.Streams.Stream_Element_Count := 0) return Natural;

         -------------
         -- Get_Int --
         -------------

         function Get_Int
           (From : Ada.Streams.Stream_Element_Count := 0) return Natural
         is
            subtype X is Natural;
         begin
            return
              ((X (Data (Data'First + From)) * 256
               + X (Data (Data'First + From + 1))) * 256
               + X (Data (Data'First + From + 2))) * 256
               + X (Data (Data'First + From + 3));
         end Get_Int;

         Index : Natural;
      begin
         Ada.Text_IO.Put ("MSG:" & GNAT.Sockets.Image (Self.Peer) & " ");

         case Data (Data'First) is
            when 0 =>  --  choke
               Ada.Text_IO.Put_Line ("choke");
               Self.We_Choked := True;
            when 1 =>  -- unchoke
               Ada.Text_IO.Put_Line ("unchoke");
               Self.We_Choked := False;
               Send_Requests;
            when 2 =>  -- interested
               Ada.Text_IO.Put_Line ("interested");
               Self.He_Intrested := True;
            when 3 =>  -- not interested
               Ada.Text_IO.Put_Line ("not interested");
               Self.He_Intrested := False;
            when 4 =>  -- have

               declare
                  Index : constant Positive := Get_Int (1) + 1;
               begin
                  if Index in Self.Piece_Map'Range then
                     Ada.Text_IO.Put_Line ("have" & (Index'Img));

                     Self.Piece_Map (Index) := True;
                     Check_Intrested;
                  end if;
               end;
            when 5 => -- bitfield
               Index := 1;
               Ada.Text_IO.Put_Line ("bitfield");

               Each_Byte :
               for X of Data (Data'First + 1 .. Data'Last) loop
                  declare
                     use type Interfaces.Unsigned_8;
                     Byte : Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (X);
                  begin
                     for J in 1 .. 8 loop
                        if (Byte and 16#80#) /= 0 then
                           Self.Piece_Map (Index) := True;
                        end if;

                        Byte := Interfaces.Shift_Left (Byte, 1);
                        Index := Index + 1;

                        exit Each_Byte when Index > Self.Piece_Count;
                     end loop;
                  end;
               end loop Each_Byte;

               Check_Intrested;

            when 6 => -- request
               declare
                  Next : constant Request :=
                    (Index  => Get_Int (1) + 1,
                     Offset => Get_Int (5),
                     Length => Get_Int (9));
               begin
                  Ada.Text_IO.Put_Line
                    ("request" & (Next.Index'Img) & (Next.Offset'Img));

                  if Index in Self.Piece_Map'Range
                    and then not Self.He_Choked
                    and then Self.Last_Request < Self.Requests'Last
                  then
                     Self.Last_Request := Self.Last_Request + 1;
                     Self.Requests (Self.Last_Request) := Next;
                  end if;
               end;

            when 7 => -- piece
               declare
                  Index  : constant Positive := Get_Int (1) + 1;
                  Offset : constant Natural := Get_Int (5);
               begin
                  Ada.Text_IO.Put_Line ("piece" & (Index'Img) & (Offset'Img));

                  if Index in Self.Piece_Map'Range
                    and then Data'Length > 9
                  then
                     Save_Piece
                       (Index, Offset, Data (Data'First + 9 .. Data'Last));
                  end if;
               end;

            when 8 => -- cancel
               declare
                  Next : constant Request :=
                    (Index  => Get_Int (1) + 1,
                     Offset => Get_Int (5),
                     Length => Get_Int (9));
               begin
                  Ada.Text_IO.Put_Line
                    ("cancel" & (Next.Index'Img) & (Next.Offset'Img));

                  for J in 1 .. Self.Last_Request loop
                     if Self.Requests (J) = Next then
                        if J /= Self.Last_Request then
                           Self.Requests (J) :=
                             Self.Requests (Self.Last_Request);
                        end if;

                        Self.Last_Request := Self.Last_Request - 1;

                        exit;
                     end if;
                  end loop;
               end;

            when others =>
               Ada.Text_IO.Put_Line
                 ("unkown" & (Data (Data'First)'Img));
         end case;
      end On_Message;

      -------------------
      -- Read_Messages --
      -------------------

      procedure Read_Messages
        (Data : in out Ada.Streams.Stream_Element_Array;
         Last : out Ada.Streams.Stream_Element_Count)
      is
         From   : Ada.Streams.Stream_Element_Count := Data'First;
         Length : Ada.Streams.Stream_Element_Count;
      begin
         loop
            exit when Data'Length - From + 1 < 4;

            Length := Get_Length (Data (From .. Data'Last));

            exit when Data'Length - From + 1 < 4 + Length;

            From := From + 4;

            if Length > 0 then
               On_Message (Data (From .. From + Length - 1));
               From := From + Length;
            end if;
         end loop;

         if From > Data'First then
            Last := Data'Length - From + 1;
            Data (1 .. Last) := Data (From .. Data'Last);
         else
            Last := Data'Last;
         end if;
      end Read_Messages;

      ----------------
      -- Save_Piece --
      ----------------

      procedure Save_Piece
        (Index  : Positive;
         Offset : Natural;
         Data   : Ada.Streams.Stream_Element_Array) is
      begin
         Self.Storage.Write
           (Offset => Ada.Streams.Stream_Element_Count (Index - 1)
                        * Self.Meta.Piece_Length
                      + Ada.Streams.Stream_Element_Count (Offset),
            Data => Data);
      end Save_Piece;

      -------------------
      -- Send_Requests --
      -------------------

      procedure Send_Requests is
         subtype Int_Buffer is Ada.Streams.Stream_Element_Array (1 .. 4);
         function Get_Int (Value : Natural) return Int_Buffer;

         -------------
         -- Get_Int --
         -------------

         function Get_Int (Value : Natural) return Int_Buffer is
            use type Interfaces.Unsigned_32;
            Result : Int_Buffer;
            Next : Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Value);
         begin
            for X of reverse Result loop
               X := Ada.Streams.Stream_Element (Next mod 256);
               Next := Interfaces.Shift_Right (Next, 8);
            end loop;
            return Result;
         end Get_Int;
      begin
         if Self.Current_Piece.Intervals.Is_Empty then
            Self.Listener.Reserve_Intervals
              (Connection => Self'Unchecked_Access,
               Map        => Self.Piece_Map,
               Value      => Self.Current_Piece);
         end if;

         for J in reverse 1 .. Self.Current_Piece.Intervals.Last_Index loop
            declare
               Index  : constant Positive := Self.Current_Piece.Piece;
               Offset : constant Piece_Offset :=
                 Self.Current_Piece.Intervals (J).From;
               Length : constant Piece_Offset :=
                 Self.Current_Piece.Intervals (J).To - Offset + 1;
            begin
               Self.Send_Message
                 ((00, 00, 00, 13, 06) &
                    Get_Int (Index - 1) &
                    Get_Int (Natural (Offset)) &
                    Get_Int (Natural (Length)));
               --  FIXME Check buffer full
            end;
         end loop;

         Self.Current_Piece.Intervals.Clear;
      end Send_Requests;

      Limit : constant Ada.Calendar.Time := Ada.Calendar.Clock + Time;
      Last  : Ada.Streams.Stream_Element_Count := Self.Unparsed.Length;
      Data  : Ada.Streams.Stream_Element_Array (1 .. 20_000);
   begin
      if Self.Closed then
         return;

      elsif not Self.Sent_Handshake then
         Connect_And_Send_Handshake;

         if Self.Closed then
            return;
         end if;
      end if;

      if not Self.Choked_Sent then
         Self.Choked_Sent := True;

         if Self.He_Choked then
            Self.Send_Message ((00, 00, 00, 01, 00));  --  choke
         else
            Self.Send_Message ((00, 00, 00, 01, 01));  --  unchoke
         end if;
      end if;

      Data (1 .. Last) := Self.Unparsed.To_Stream_Element_Array;
      Self.Unparsed.Clear;

      loop
         declare
            Read : Ada.Streams.Stream_Element_Count;
         begin

            GNAT.Sockets.Receive_Socket
              (Socket => Self.Socket,
               Item   => Data (Last + 1 .. Data'Last),
               Last   => Read);

            if Read = Last then
               Ada.Text_IO.Put_Line
                 ("Closed on Read:" & GNAT.Sockets.Image (Self.Peer));
               GNAT.Sockets.Close_Socket (Self.Socket);
               Self.Closed := True;
               return;
            else
               Ada.Text_IO.Put ("Read:" & GNAT.Sockets.Image (Self.Peer));
               Ada.Text_IO.Put_Line (Piece_Offset'Image (Read - Last));
               Last := Read;
            end if;

         exception
            when E : GNAT.Sockets.Socket_Error =>

               if GNAT.Sockets.Resolve_Exception (E) in
                 GNAT.Sockets.Resource_Temporarily_Unavailable
               then
                  Ada.Text_IO.Put_Line
                    ("Timeout on Read:" & GNAT.Sockets.Image (Self.Peer));
               else
                  Ada.Text_IO.Put_Line
                    ("Raised on Read:" & GNAT.Sockets.Image (Self.Peer));
                  Ada.Text_IO.Put_Line
                    (Ada.Exceptions.Exception_Information (E));

                  GNAT.Sockets.Close_Socket (Self.Socket);
                  Self.Closed := True;
                  return;
               end if;
         end;

         if Self.Got_Handshake then

            Read_Messages (Data (1 .. Last), Last);
         elsif Last >= Handshake_Image'Length
           and then Get_Handshake (Data (1 .. Last))
         then
            Data (1 .. Last - Handshake_Image'Length) :=
              Data (Handshake_Image'Last + 1 .. Last);
            Last := Last - Handshake_Image'Length;
            Read_Messages (Data (1 .. Last), Last);

         end if;

         exit when Limit < Ada.Calendar.Clock;
      end loop;

      if Last > 0 then
         Self.Unparsed.Clear;
         Self.Unparsed.Append (Data (1 .. Last));
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Raised on Serve:" & GNAT.Sockets.Image (Self.Peer));
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information (E));
         Self.Closed := True;
         return;
   end Serve;

end Torrent.Connections;
