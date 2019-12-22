--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;

with GNAT.SHA1;

with Torrent.Handshakes; use Torrent.Handshakes;

package body Torrent.Connections is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Array;
   use type Ada.Streams.Stream_Element_Offset;

   subtype Int_Buffer is Ada.Streams.Stream_Element_Array (1 .. 4);
   function To_Int (Value : Natural) return Int_Buffer;

   Expire_Loops : constant := 3;
   --  Protection from lost of requests

   function Get_Handshake (Self : Connection'Class) return Handshake_Image;

   function Get_Int
     (Data : Ada.Streams.Stream_Element_Array;
      From : Ada.Streams.Stream_Element_Count := 0) return Natural;

   function Is_Valid_Piece
     (Self  : Connection'Class;
      Piece : Piece_Index) return Boolean;

   procedure Send_Message
     (Self : in out Connection'Class;
      Data : Ada.Streams.Stream_Element_Array);

   procedure Send_Have
     (Self  : in out Connection'Class;
      Piece : Piece_Index);

   procedure Send_Bitfield
     (Self      : in out Connection'Class;
      Completed : Piece_Index_Array);

   procedure Send_Pieces (Self : in out Connection'Class);
   procedure Unreserve_Intervals (Self : in out Connection'Class);

   procedure Close_Connection (Self : in out Connection'Class);

   ----------------------
   -- Close_Connection --
   ----------------------

   procedure Close_Connection (Self : in out Connection'Class) is
   begin
      GNAT.Sockets.Close_Socket (Self.Socket);
      GNAT.Sockets.Close_Selector (Self.Selector);
      Self.Closed := True;
      Self.Unreserve_Intervals;
   end Close_Connection;

   ---------------
   -- Connected --
   ---------------

   function Connected (Self : Connection'Class) return Boolean is
   begin
      return not Self.Closed;
   end Connected;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake
     (Self      : in out Connection'Class;
      Socket    : GNAT.Sockets.Socket_Type;
      Completed : Piece_Index_Array;
      Inbound   : Boolean)
   is
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      Self.Socket := Socket;

      GNAT.Sockets.Send_Socket
        (Socket => Self.Socket,
         Item   => Self.Get_Handshake,
         Last   => Last);

      pragma Assert (Last = Handshake_Image'Last);

      GNAT.Sockets.Set_Socket_Option
        (Socket => Self.Socket,
         Level  => GNAT.Sockets.Socket_Level,
         Option => (GNAT.Sockets.Receive_Timeout, 0.1));

      GNAT.Sockets.Set_Socket_Option
        (Socket => Self.Socket,
         Level  => GNAT.Sockets.Socket_Level,
         Option => (GNAT.Sockets.Send_Timeout, 0.0));

      GNAT.Sockets.Create_Selector (Self.Selector);

      Self.Initialize (Self.My_Peer_Id, Self.Peer, Self.Listener);
      Self.Sent_Handshake := True;
      Self.Got_Handshake := Inbound;

      Self.Send_Bitfield (Completed);
      Self.Last_Completed := Completed'Last;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Raised on Do_Handshake:" & GNAT.Sockets.Image (Self.Peer));
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information (E));
         Self.Close_Connection;
         return;
   end Do_Handshake;

   -------------------
   -- Get_Handshake --
   -------------------

   function Get_Handshake (Self : Connection'Class) return Handshake_Image is
      Result : Handshake_Type;

   begin
      Result.Info_Hash := Self.Meta.Info_Hash;
      Result.Peer_Id := Self.My_Peer_Id;

      return +Result;
   end Get_Handshake;

   -------------
   -- Get_Int --
   -------------

   function Get_Int
     (Data : Ada.Streams.Stream_Element_Array;
      From : Ada.Streams.Stream_Element_Count := 0) return Natural
   is
      subtype X is Natural;
   begin
      return
        ((X (Data (Data'First + From)) * 256
         + X (Data (Data'First + From + 1))) * 256
         + X (Data (Data'First + From + 2))) * 256
        + X (Data (Data'First + From + 3));
   end Get_Int;

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
      Self.Last_Completed := 0;
      Self.Listener := Listener;
      Self.Current_Piece := (0, Intervals => <>);
      Self.Piece_Map := (others => False);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List  : in out Interval_Vectors.Vector;
      Value : Interval)
   is
      M, N : Natural := 0;
      Next : Interval := Value;
   begin
      if (for some X of List => X.From <= Next.From and X.To >= Next.To) then
         return;
      end if;

      loop
         M := 0;
         N := 0;

         for J in 1 .. List.Last_Index loop
            if List (J).To + 1 = Next.From then
               M := J;
               exit;
            elsif Next.To + 1 = List (J).From then
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
   end Insert;

   ---------------
   -- Intrested --
   ---------------

   function Intrested (Self : Connection'Class) return Boolean is
   begin
      return not Self.Closed and Self.He_Intrested;
   end Intrested;

   --------------------
   -- Is_Valid_Piece --
   --------------------

   function Is_Valid_Piece
     (Self  : Connection'Class;
      Piece : Piece_Index) return Boolean is
   begin
      return Is_Valid_Piece (Self.Meta, Self.Storage.all, Piece);
   end Is_Valid_Piece;

   --------------------
   -- Is_Valid_Piece --
   --------------------

   function Is_Valid_Piece
     (Meta    : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      Storage : in out Torrent.Storages.Storage;
      Piece   : Piece_Index) return Boolean
   is
      Context : GNAT.SHA1.Context;
      From    : Ada.Streams.Stream_Element_Offset :=
        Piece_Offset (Piece - 1) * Meta.Piece_Length;
      Left    : Ada.Streams.Stream_Element_Offset;
      Data    : Ada.Streams.Stream_Element_Array (1 .. Max_Interval_Size);
      Value   : SHA1;
   begin
      Storage.Start_Reading;  --  Block storage

      if Piece = Meta.Piece_Count then
         Left := Meta.Last_Piece_Length;
      else
         Left := Meta.Piece_Length;
      end if;

      while Left > Data'Length loop
         Storage.Read (From, Data);
         From := From + Data'Length;
         Left := Left - Data'Length;
         GNAT.SHA1.Update (Context, Data);
      end loop;

      if Left > 0 then
         Storage.Read (From, Data (1 .. Left));
         GNAT.SHA1.Update (Context, Data (1 .. Left));
      end if;

      Value := GNAT.SHA1.Digest (Context);

      Storage.Stop_Reading;  --  Unblock storage

      return Meta.Piece_SHA1 (Piece) = Value;
   end Is_Valid_Piece;

   ----------
   -- Peer --
   ----------

   function Peer
     (Self : Connection'Class) return GNAT.Sockets.Sock_Addr_Type is
   begin
      return Self.Peer;
   end Peer;

   -------------------
   -- Send_Bitfield --
   -------------------

   procedure Send_Bitfield
     (Self      : in out Connection'Class;
      Completed : Piece_Index_Array)
   is
      Length : constant Piece_Offset :=
        Piece_Offset (Self.Piece_Count + 7) / 8;
      Data   : Ada.Streams.Stream_Element_Array (0 .. Length - 1) :=
        (others => 0);  --  Zero based
      Mask   : Interfaces.Unsigned_8;
   begin
      for X of Completed loop
         Mask := Interfaces.Shift_Right (128, Natural ((X - 1) mod 8));

         Data (Piece_Offset (X - 1) / 8) := Data (Piece_Offset (X - 1) / 8)
           or Ada.Streams.Stream_Element (Mask);
      end loop;

      Self.Send_Message (To_Int (Positive (Length) + 1) & 05 & Data);
   end Send_Bitfield;

   ---------------
   -- Send_Have --
   ---------------

   procedure Send_Have
     (Self  : in out Connection'Class;
      Piece : Piece_Index) is
   begin
      Self.Send_Message
        ((00, 00, 00, 05, 04) &   --  have
           To_Int (Natural (Piece - 1)));
   end Send_Have;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Self : in out Connection'Class;
      Data : Ada.Streams.Stream_Element_Array)
   is
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      if Self.Closed then
         return;
      end if;

      GNAT.Sockets.Send_Socket
        (Socket => Self.Socket,
         Item   => Data,
         Last   => Last);

      pragma Assert (Last = Data'Last);

      if Data'Length <= 4 then
         Ada.Text_IO.Put_Line
           ("Send keepalive " & GNAT.Sockets.Image (Self.Peer));
      elsif Data (Data'First + 4) = 6 then
         Ada.Text_IO.Put_Line
           ("Send request "
            & GNAT.Sockets.Image (Self.Peer)
            & Integer'Image (Get_Int (Data, 5) + 1)
            & Integer'Image (Get_Int (Data, 9))
            & Integer'Image (Get_Int (Data, 13)));
      else
         Ada.Text_IO.Put_Line
           ("Send "
            & GNAT.Sockets.Image (Self.Peer)
            & (Data (Data'First + 4)'Img));
      end if;
   exception
      when E : GNAT.Sockets.Socket_Error =>

         if GNAT.Sockets.Resolve_Exception (E) in
           GNAT.Sockets.Resource_Temporarily_Unavailable
         then
            --  If timeout then wait until the socket is ready to transmit and
            --  try again
            declare
               Status : GNAT.Sockets.Selector_Status;
               R_Set  : GNAT.Sockets.Socket_Set_Type;
               W_Set  : GNAT.Sockets.Socket_Set_Type;
            begin
               GNAT.Sockets.Set (W_Set, Self.Socket);

               GNAT.Sockets.Check_Selector
                 (Selector     => Self.Selector,
                  R_Socket_Set => R_Set,
                  W_Socket_Set => W_Set,
                  Status       => Status);

               if Status in GNAT.Sockets.Completed then
                  Self.Send_Message (Data);
                  return;
               end if;
            end;
         end if;

         Ada.Text_IO.Put_Line
           ("Send_Message:" & GNAT.Sockets.Image (Self.Peer));
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information (E));
         Self.Close_Connection;
   end Send_Message;

   -----------------
   -- Send_Pieces --
   -----------------

   procedure Send_Pieces (Self : in out Connection'Class) is
   begin  --  FIXME check if unchoked
      while not Self.Requests.Is_Empty loop
         declare
            Last : Ada.Streams.Stream_Element_Count;
            Item : constant Piece_Interval := Self.Requests.Last_Element;
            Data : Ada.Streams.Stream_Element_Array
              (Item.Span.From - 4 - 1 - 4 - 4 .. Item.Span.To);
         begin
            Self.Storage.Start_Reading;
            Self.Storage.Read
              (Offset => Ada.Streams.Stream_Element_Count (Item.Piece - 1)
                        * Self.Meta.Piece_Length
               + Ada.Streams.Stream_Element_Count (Item.Span.From),
               Data   => Data (Item.Span.From .. Item.Span.To));
            Self.Storage.Stop_Reading;

            Data (Data'First .. Data'First + 3) :=  --  Message length
              To_Int (Data'Length - 4);
            Data (Data'First + 4) := 7;  --  piece
            Data (Data'First + 5 .. Data'First + 8) :=
              To_Int (Positive (Item.Piece) - 1);
            Data (Data'First + 9 .. Data'First + 12) :=
              To_Int (Natural (Item.Span.From));

            if Self.Closed then
               return;
            end if;

            GNAT.Sockets.Send_Socket
              (Socket => Self.Socket,
               Item   => Data,
               Last   => Last);

            pragma Assert (Last = Data'Last);

            Ada.Text_IO.Put_Line
              ("Send piece "
               & GNAT.Sockets.Image (Self.Peer)
               & Piece_Count'Image (Item.Piece)
               & Piece_Offset'Image (Item.Span.From));

            Self.Requests.Delete_Last;
            --   FIXME Increment send statistic.
         end;
      end loop;
   exception
      when E : GNAT.Sockets.Socket_Error =>

         if GNAT.Sockets.Resolve_Exception (E) in
           GNAT.Sockets.Resource_Temporarily_Unavailable
         then
            return;
         end if;

         Ada.Text_IO.Put_Line
           ("Send_Pieces:" & GNAT.Sockets.Image (Self.Peer));
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information (E));
         Self.Close_Connection;
   end Send_Pieces;

   -----------
   -- Serve --
   -----------

   procedure Serve
     (Self      : in out Connection'Class;
      Completed : Piece_Index_Array;
      Time      : Duration)
   is
      use type Ada.Calendar.Time;

      procedure Check_Intrested;
      procedure Send_Initial_Requests;
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
        (Index  : Piece_Index;
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
         end if;
      end Check_Intrested;

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
           (From : Ada.Streams.Stream_Element_Count := 0) return Natural
             is (Get_Int (Data, From));

         Index : Piece_Index;
      begin
         Ada.Text_IO.Put ("MSG:" & GNAT.Sockets.Image (Self.Peer) & " ");

         case Data (Data'First) is
            when 0 =>  --  choke
               Ada.Text_IO.Put_Line ("choke");
               Self.We_Choked := True;
               Self.Unreserve_Intervals;
            when 1 =>  -- unchoke
               Ada.Text_IO.Put_Line ("unchoke");
               Self.We_Choked := False;
               Send_Initial_Requests;
            when 2 =>  -- interested
               Ada.Text_IO.Put_Line ("interested");
               Self.He_Intrested := True;
            when 3 =>  -- not interested
               Ada.Text_IO.Put_Line ("not interested");
               Self.He_Intrested := False;
            when 4 =>  -- have

               declare
                  Index : constant Piece_Index :=
                    Piece_Index (Get_Int (1) + 1);
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
                  Next : Piece_Interval :=
                    (Piece  => Piece_Count (Get_Int (1) + 1),
                     Span   => (From => Piece_Offset (Get_Int (5)),
                                To   => Piece_Offset (Get_Int (9))));
               begin
                  Ada.Text_IO.Put_Line
                    ("request" & (Next.Piece'Img) & (Next.Span.From'Img));

                  if Next.Span.To > Max_Interval_Size then
                     return;
                  else
                     Next.Span.To := Next.Span.From + Next.Span.To - 1;
                  end if;

                  if Next.Piece in Self.Piece_Map'Range
                    and then not Self.He_Choked
                  then
                     Self.Requests.Append (Next);
                  end if;
               end;

            when 7 => -- piece
               declare
                  Index  : constant Piece_Index :=
                    Piece_Index (Get_Int (1) + 1);
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
                  Next : Piece_Interval :=
                    (Piece  => Piece_Count (Get_Int (1) + 1),
                     Span   => (From => Piece_Offset (Get_Int (5)),
                                To   => Piece_Offset (Get_Int (9))));
                  Cursor : Natural;
               begin
                  Next.Span.To := Next.Span.From + Next.Span.To - 1;
                  Ada.Text_IO.Put_Line
                    ("cancel" & (Next.Piece'Img) & (Next.Span.From'Img));

                  Cursor := Self.Requests.Find_Index (Next);

                  if Cursor /= 0 then
                     Self.Requests.Swap (Cursor, Self.Requests.Last_Index);
                     Self.Requests.Delete_Last;
                  end if;
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
            else
               Self.Send_Message ((00, 00, 00, 00));  --  keepalive
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
        (Index  : Piece_Index;
         Offset : Natural;
         Data   : Ada.Streams.Stream_Element_Array)
      is
         procedure Swap (Left, Right : Piece_Interval_Count);

         ----------
         -- Swap --
         ----------

         procedure Swap (Left, Right : Piece_Interval_Count) is
            Request : constant Piece_Interval :=
              Self.Pipelined.Request.List (Left);
            Expire : constant Natural := Self.Pipelined.Expire (Left);
         begin
            Self.Pipelined.Request.List (Left) :=
              Self.Pipelined.Request.List (Right);
            Self.Pipelined.Request.List (Right) := Request;
            Self.Pipelined.Expire (Left) := Self.Pipelined.Expire (Right);
            Self.Pipelined.Expire (Right) := Expire;
         end Swap;

         Last : Boolean;
         From : constant Piece_Offset := Piece_Offset (Offset);
         J    : Natural := 1;
      begin
         Self.Storage.Write
           (Offset => Ada.Streams.Stream_Element_Count (Index - 1)
                        * Self.Meta.Piece_Length
                      + Ada.Streams.Stream_Element_Count (Offset),
            Data   => Data);

         Self.Listener.Interval_Saved
           (Index, (From, From + Data'Length - 1), Last);

         if Last then
            if Self.Is_Valid_Piece (Index) then
               Ada.Text_IO.Put_Line ("Piece completed" & (Index'Img));

               Self.Listener.Piece_Completed (Index, True);
               Self.Send_Have (Index);
            else
               Ada.Text_IO.Put_Line ("Piece FAILED!" & (Index'Img));
               Self.Listener.Piece_Completed (Index, False);
            end if;
         end if;

         if Self.Current_Piece.Intervals.Is_Empty then
            Self.Listener.Reserve_Intervals
              (Map   => Self.Piece_Map,
               Value => Self.Current_Piece);
         end if;

         while J <= Self.Pipelined.Length loop
            if Self.Pipelined.Request.List (J).Piece = Index
              and Self.Pipelined.Request.List (J).Span.From = From
            then
               if Self.Current_Piece.Intervals.Is_Empty then
                  Swap (J, Self.Pipelined.Length);

                  Self.Pipelined :=
                    (Self.Pipelined.Length - 1,
                     Request =>
                       (Self.Pipelined.Length - 1,
                        Self.Pipelined.Request.List
                          (1 .. Self.Pipelined.Length - 1)),
                     Expire  => Self.Pipelined.Expire
                       (1 .. Self.Pipelined.Length - 1));
               else
                  declare
                     Last : constant Interval :=
                       Self.Current_Piece.Intervals.Last_Element;
                  begin
                     Self.Current_Piece.Intervals.Delete_Last;

                     Self.Pipelined.Request.List (J) :=
                       (Self.Current_Piece.Piece, Last);

                     Self.Pipelined.Expire (J) :=
                       Self.Pipelined.Length * Expire_Loops;

                     Self.Send_Message
                       ((00, 00, 00, 13, 06) &
                          To_Int (Natural (Self.Current_Piece.Piece - 1)) &
                          To_Int (Natural (Last.From)) &
                          To_Int (Natural (Last.To - Last.From + 1)));

                     J := J + 1;
                  end;
               end if;

            else  --  Check if some request was lost
               Self.Pipelined.Expire (J) := Self.Pipelined.Expire (J) - 1;

               if Self.Pipelined.Expire (J) = 0 then
                  Ada.Text_IO.Put_Line ("Re-send lost request:");

                  Self.Pipelined.Expire (J) :=
                    Self.Pipelined.Length * Expire_Loops;

                  Self.Send_Message
                    ((00, 00, 00, 13, 06) &
                       To_Int (Natural
                                (Self.Pipelined.Request.List (J).Piece - 1)) &
                       To_Int (Natural (Self.Pipelined.Request.List (J)
                                             .Span.From)) &
                       To_Int (Natural (Self.Pipelined.Request.List (J)
                                             .Span.To
                                        - Self.Pipelined.Request.List (J)
                                             .Span.From + 1)));
               end if;

               J := J + 1;
            end if;
         end loop;
      end Save_Piece;

      ---------------------------
      -- Send_Initial_Requests --
      ---------------------------

      procedure Send_Initial_Requests is
         Length : Piece_Interval_Count;
      begin
         if Self.Current_Piece.Intervals.Is_Empty then
            Self.Listener.Reserve_Intervals
              (Map        => Self.Piece_Map,
               Value      => Self.Current_Piece);
         end if;

         Length := Piece_Interval_Count'Min
           (Piece_Interval_Count'Last,
            Self.Current_Piece.Intervals.Last_Index);

         Self.Pipelined :=
           (Length => Length,
            Expire => (1 .. Length => Length * Expire_Loops),
            Request => (Length, others => <>));

         for J in 1 .. Length loop
            declare
               Last   : constant Interval :=
                 Self.Current_Piece.Intervals.Last_Element;
               Index  : constant Piece_Index := Self.Current_Piece.Piece;
               Offset : constant Piece_Offset := Last.From;
               Length : constant Piece_Offset := Last.To - Offset + 1;
            begin
               Self.Send_Message
                 ((00, 00, 00, 13, 06) &
                    To_Int (Natural (Index - 1)) &
                    To_Int (Natural (Offset)) &
                    To_Int (Natural (Length)));

               Self.Pipelined.Request.List (J) :=
                 (Piece => Self.Current_Piece.Piece,
                  Span  => (Offset, Last.To));

               Self.Current_Piece.Intervals.Delete_Last;
            end;
         end loop;
      end Send_Initial_Requests;

      Limit : constant Ada.Calendar.Time := Ada.Calendar.Clock + Time;
      Last  : Ada.Streams.Stream_Element_Count := Self.Unparsed.Length;
      Data  : Ada.Streams.Stream_Element_Array (1 .. 20_000);
   begin
      if Self.Closed then
         return;
      end if;

      if not Self.Choked_Sent then
         Self.Choked_Sent := True;

         if Self.He_Choked then
            Self.Send_Message ((00, 00, 00, 01, 00));  --  choke
            Self.Requests.Clear;
         else
            Self.Send_Message ((00, 00, 00, 01, 01));  --  unchoke
         end if;
      end if;

      for J in Self.Last_Completed + 1 .. Completed'Last loop
         Self.Send_Have (Completed (J));
      end loop;

      Self.Last_Completed := Completed'Last;

      Data (1 .. Last) := Self.Unparsed.To_Stream_Element_Array;
      Self.Unparsed.Clear;

      while Limit >= Ada.Calendar.Clock loop
         declare
            Read : Ada.Streams.Stream_Element_Count;
         begin

            if Self.Closed then
               return;
            end if;

            GNAT.Sockets.Receive_Socket
              (Socket => Self.Socket,
               Item   => Data (Last + 1 .. Data'Last),
               Last   => Read);

            if Read = Last then
               Ada.Text_IO.Put_Line
                 ("Closed on Read:" & GNAT.Sockets.Image (Self.Peer));
               Self.Close_Connection;
               return;
            else
               Last := Read;
            end if;

         exception
            when E : GNAT.Sockets.Socket_Error =>

               if GNAT.Sockets.Resolve_Exception (E) in
                 GNAT.Sockets.Resource_Temporarily_Unavailable
               then
                  null;  --  Timeout on Read
               else
                  Ada.Text_IO.Put_Line
                    ("Raised on Read:" & GNAT.Sockets.Image (Self.Peer));
                  Ada.Text_IO.Put_Line
                    (Ada.Exceptions.Exception_Information (E));

                  Self.Close_Connection;
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

         Self.Send_Pieces;
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
         Self.Close_Connection;
         return;
   end Serve;

   ----------------
   -- Set_Choked --
   ----------------

   procedure Set_Choked (Self : in out Connection'Class; Value : Boolean) is
   begin
      if Self.He_Choked /= Value then
         Self.He_Choked := Value;
         Self.Choked_Sent := not Self.Choked_Sent;
      end if;
   end Set_Choked;

   -------------
   -- To_Int --
   -------------

   function To_Int (Value : Natural) return Int_Buffer is
      use type Interfaces.Unsigned_32;
      Result : Int_Buffer;
      Next : Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Value);
   begin
      for X of reverse Result loop
         X := Ada.Streams.Stream_Element (Next mod 256);
         Next := Interfaces.Shift_Right (Next, 8);
      end loop;
      return Result;
   end To_Int;

   -------------------------
   -- Unreserve_Intervals --
   -------------------------

   procedure Unreserve_Intervals (Self : in out Connection'Class) is
      Back : Piece_Interval_Array
        (1 .. Self.Current_Piece.Intervals.Last_Index);
   begin
      for J in Back'Range loop
         Back (J).Piece := Self.Current_Piece.Piece;
         Back (J).Span := Self.Current_Piece.Intervals (J);
      end loop;

      Self.Listener.Unreserve_Intervals (Self.Pipelined.Request.List & Back);

      Self.Current_Piece := (0, Interval_Vectors.Empty_Vector);
      Self.Pipelined := (0, Request => (0, others => <>), Expire => <>);
   end Unreserve_Intervals;

end Torrent.Connections;
