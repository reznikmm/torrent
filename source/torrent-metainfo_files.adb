--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with GNAT.SHA1;

with League.Stream_Element_Vectors;
with League.Text_Codecs;

package body Torrent.Metainfo_Files is

   procedure Free is new Ada.Unchecked_Deallocation
     (Metainfo, Metainfo_Access);

   --------------
   -- Announce --
   --------------

   not overriding function Announce
     (Self : Metainfo_File) return League.IRIs.IRI is
   begin
      return Self.Data.Announce;
   end Announce;

   ----------------
   -- File_Count --
   ----------------

   not overriding function File_Count (Self : Metainfo_File) return Positive is
   begin
      return Self.Data.File_Count;
   end File_Count;

   -----------------
   -- File_Length --
   -----------------

   not overriding function File_Length
     (Self  : Metainfo_File;
      Index : Positive) return Ada.Streams.Stream_Element_Count is
   begin
      return Self.Data.Files (Index).Length;
   end File_Length;

   ---------------
   -- File_Path --
   ---------------

   not overriding function File_Path
     (Self  : Metainfo_File;
      Index : Positive) return League.String_Vectors.Universal_String_Vector
   is
   begin
      return Self.Data.Files (Index).Path;
   end File_Path;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Metainfo_File) is
   begin
      Free (Self.Data);
   end Finalize;

   ---------------
   -- Info_Hash --
   ---------------

   not overriding function Info_Hash (Self  : Metainfo_File) return SHA1 is
   begin
      return Self.Data.Info_Hash;
   end Info_Hash;

   ----------
   -- Name --
   ----------

   not overriding function Name
     (Self : Metainfo_File) return League.Strings.Universal_String is
   begin
      return Self.Data.Name;
   end Name;

   -----------------
   -- Piece_Count --
   -----------------

   not overriding function Piece_Count
     (Self : Metainfo_File) return Positive is
   begin
      return Self.Data.Piece_Count;
   end Piece_Count;

   ------------------
   -- Piece_Length --
   ------------------

   not overriding function Piece_Length
     (Self : Metainfo_File) return Ada.Streams.Stream_Element_Count is
   begin
      return Self.Data.Piece_Length;
   end Piece_Length;

   ----------------
   -- Piece_SHA1 --
   ----------------

   not overriding function Piece_SHA1
     (Self  : Metainfo_File;
      Index : Positive) return SHA1
   is
   begin
      return Self.Data.Hashes (Index);
   end Piece_SHA1;

   ----------
   -- Read --
   ----------

   not overriding procedure Read
     (Self      : in out Metainfo_File;
      File_Name : League.Strings.Universal_String)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      use type League.Strings.Universal_String;

      package File_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => File_Information);

      function "+"
        (Text : Wide_Wide_String) return League.Strings.Universal_String
           renames League.Strings.To_Universal_String;

      function Parse_Top_Dictionary return Metainfo;

      procedure Parse_Int (Value : out Ada.Streams.Stream_Element_Offset);

      procedure Parse_String (Value : out League.Strings.Universal_String);

      procedure Parse_Pieces
        (Value : out League.Stream_Element_Vectors.Stream_Element_Vector);

      procedure Parse_String_List
        (Value : out League.String_Vectors.Universal_String_Vector);

      procedure Skip_Value;
      procedure Skip_List;
      procedure Skip_Dictionary;
      procedure Skip_String;
      procedure Skip_Int;

      procedure Parse_Files (Value : out File_Vectors.Vector);

      procedure Expect (Char : Ada.Streams.Stream_Element);

      procedure Parse_File
        (Path   : out League.String_Vectors.Universal_String_Vector;
         Length : out Ada.Streams.Stream_Element_Count);

      procedure Parse_Info
        (Name      : out League.Strings.Universal_String;
         Piece_Len : out Ada.Streams.Stream_Element_Count;
         Files     : out File_Vectors.Vector;
         Pieces    : out League.Stream_Element_Vectors.Stream_Element_Vector);

      procedure Read_Buffer;

      subtype Digit is Ada.Streams.Stream_Element
        range Character'Pos ('0') .. Character'Pos ('9');

      Input    : Ada.Streams.Stream_IO.File_Type;
      Buffer   : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last     : Ada.Streams.Stream_Element_Count := 0;
      Next     : Ada.Streams.Stream_Element_Count := 1;
      Error    : constant String := "Can't parse torrent file.";
      SHA_From : Ada.Streams.Stream_Element_Count := Buffer'Last + 1;
      Context  : GNAT.SHA1.Context := GNAT.SHA1.Initial_Context;
      Codec    : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");

      package Constants is
         Announce : constant League.Strings.Universal_String := +"announce";
         Files    : constant League.Strings.Universal_String := +"files";
         Info     : constant League.Strings.Universal_String := +"info";
         Length   : constant League.Strings.Universal_String := +"length";
         Name     : constant League.Strings.Universal_String := +"name";
         Path     : constant League.Strings.Universal_String := +"path";
         Pieces   : constant League.Strings.Universal_String := +"pieces";

--           Announce_List : constant League.Strings.Universal_String :=
--             +"announce-list";
         Piece_Length : constant League.Strings.Universal_String :=
           +"piece length";
      end Constants;

      -----------------
      -- Read_Buffer --
      -----------------

      procedure Read_Buffer is
      begin
         if SHA_From <= Last then
            GNAT.SHA1.Update (Context, Buffer (SHA_From .. Last));

            SHA_From := 1;
         end if;

         Ada.Streams.Stream_IO.Read (Input, Buffer, Last);
         Next := 1;
      end Read_Buffer;

      ------------
      -- Expect --
      ------------

      procedure Expect (Char : Ada.Streams.Stream_Element) is
      begin
         if Buffer (Next) = Char then

            Next := Next + 1;

            if Next > Last then
               Read_Buffer;
            end if;
         else
            raise Constraint_Error with Error;
         end if;
      end Expect;

      ----------------
      -- Parse_File --
      ----------------

      procedure Parse_File
        (Path   : out League.String_Vectors.Universal_String_Vector;
         Length : out Ada.Streams.Stream_Element_Count)
      is
         Key : League.Strings.Universal_String;
      begin
         Expect (Character'Pos ('d'));

         while Buffer (Next) /= Character'Pos ('e') loop
            Parse_String (Key);

            if Key = Constants.Length then
               Parse_Int (Length);

            elsif Key = Constants.Path then
               Parse_String_List (Path);

            else
               Skip_Value;
            end if;
         end loop;

         Expect (Character'Pos ('e'));
      end Parse_File;

      -----------------
      -- Parse_Files --
      -----------------

      procedure Parse_Files (Value : out File_Vectors.Vector) is
         Path   : League.String_Vectors.Universal_String_Vector;
         Length : Ada.Streams.Stream_Element_Count;
      begin
         Expect (Character'Pos ('l'));

         while Buffer (Next) /= Character'Pos ('e') loop
            Parse_File (Path, Length);
            Value.Append ((Length, Path));
         end loop;

         Expect (Character'Pos ('e'));
      end Parse_Files;

      ----------------
      -- Parse_Info --
      ----------------

      procedure Parse_Info
        (Name      : out League.Strings.Universal_String;
         Piece_Len : out Ada.Streams.Stream_Element_Count;
         Files     : out File_Vectors.Vector;
         Pieces    : out League.Stream_Element_Vectors.Stream_Element_Vector)
      is
         Key    : League.Strings.Universal_String;
         Length : Ada.Streams.Stream_Element_Count := 0;
      begin
         SHA_From := Next;  --  Activate SHA1 calculation
         Expect (Character'Pos ('d'));

         while Buffer (Next) /= Character'Pos ('e') loop
            Parse_String (Key);

            if Key = Constants.Name then
               declare
                  Path : League.String_Vectors.Universal_String_Vector;
               begin
                  Parse_String (Name);

                  if Length > 0 then
                     --  There is a key length or a key files, but not both
                     --  or neither. If length is present then the download
                     --  represents a single file, otherwise it represents a
                     --  set of files which go in a directory structure.

                     Path.Append (Name);
                     Files.Append ((Length, Path));
                  end if;
               end;

            elsif Key = Constants.Piece_Length then
               Parse_Int (Piece_Len);

            elsif Key = Constants.Length then
               Parse_Int (Length);

            elsif Key = Constants.Files then
               Parse_Files (Files);

            elsif Key = Constants.Pieces then
               Parse_Pieces (Pieces);

            else

               Skip_Value;
            end if;
         end loop;

         GNAT.SHA1.Update (Context, Buffer (SHA_From .. Next));
         SHA_From := Buffer'Last + 1;  --  Deactivate SHA1 calculation

         Expect (Character'Pos ('e'));
      end Parse_Info;

      ---------------
      -- Parse_Int --
      ---------------

      procedure Parse_Int (Value : out Ada.Streams.Stream_Element_Offset) is
      begin
         Expect (Character'Pos ('i'));
         Value := 0;

         while Buffer (Next) in Digit loop
            Value := Value * 10
              + Ada.Streams.Stream_Element_Offset (Buffer (Next))
              - Character'Pos ('0');

            Expect (Buffer (Next));
         end loop;

         Expect (Character'Pos ('e'));
      end Parse_Int;

      --------------------------
      -- Parse_Top_Dictionary --
      --------------------------

      function Parse_Top_Dictionary return Metainfo is
         Index    : Ada.Streams.Stream_Element_Count;
         Key      : League.Strings.Universal_String;
         Announce : League.Strings.Universal_String;
         Name     : League.Strings.Universal_String;
         Files    : File_Vectors.Vector;
         Pieces   : League.Stream_Element_Vectors.Stream_Element_Vector;

         Piece_Length : Ada.Streams.Stream_Element_Count;
      begin
         Expect (Character'Pos ('d'));

         while Buffer (Next) /= Character'Pos ('e') loop
            Parse_String (Key);

            if Key = Constants.Announce then
               Parse_String (Announce);

            elsif Key = Constants.Info then
               Parse_Info (Name, Piece_Length, Files, Pieces);

            else

               Skip_Value;
            end if;
         end loop;

         Expect (Character'Pos ('e'));

         if Last /= 0 then
            raise Constraint_Error with Error;
         end if;

         return Result : Metainfo :=
           (Piece_Count  => Positive (Pieces.Length) / 20,
            File_Count   => Files.Last_Index,
            Announce     => League.IRIs.From_Universal_String (Announce),
            Name         => Name,
            Piece_Length => Piece_Length,
            Info_Hash    => GNAT.SHA1.Digest (Context),
            others       => <>)
         do
            for J in Result.Hashes'Range loop
               for K in SHA1'Range loop
                  Index :=
                    Ada.Streams.Stream_Element_Count (J - 1) * SHA1'Length + K;

                  Result.Hashes (J) (K) := Pieces.Element (Index);
               end loop;

               for J in 1 .. Files.Last_Index loop
                  Result.Files (J) := Files.Element (J);
               end loop;
            end loop;
         end return;
      end Parse_Top_Dictionary;

      ------------------
      -- Parse_Pieces --
      ------------------

      procedure Parse_Pieces
        (Value : out League.Stream_Element_Vectors.Stream_Element_Vector)
      is
         Len : Ada.Streams.Stream_Element_Count := 0;
      begin
         while Buffer (Next) in Digit loop
            Len := Len * 10
              + Ada.Streams.Stream_Element_Count (Buffer (Next))
              - Character'Pos ('0');

            Expect (Buffer (Next));
         end loop;

         Expect (Character'Pos (':'));

         while Last - Next + 1 <= Len loop
            Value.Append (Buffer (Next .. Last));
            Len := Len - (Last - Next + 1);
            Read_Buffer;
         end loop;

         if Len > 0 then
            Value.Append (Buffer (Next .. Next + Len - 1));
            Next := Next + Len;
         end if;
      end Parse_Pieces;

      ------------------
      -- Parse_String --
      ------------------

      procedure Parse_String (Value : out League.Strings.Universal_String) is
         Len : Ada.Streams.Stream_Element_Count := 0;
      begin
         while Buffer (Next) in Digit loop
            Len := Len * 10
              + Ada.Streams.Stream_Element_Count (Buffer (Next))
              - Character'Pos ('0');

            Expect (Buffer (Next));
         end loop;

         declare
            Data : Ada.Streams.Stream_Element_Array (1 .. Len);
            To   : Ada.Streams.Stream_Element_Count := 0;
         begin
            Expect (Character'Pos (':'));

            while Last - Next + 1 <= Len loop
               Data (To + 1 .. To + Last - Next + 1) := Buffer (Next .. Last);
               To  := To  +  Last - Next + 1;
               Len := Len - (Last - Next + 1);
               Read_Buffer;
            end loop;

            if Len > 0 then
               Data (To + 1 .. Data'Last) := Buffer (Next .. Next + Len - 1);
               Next := Next + Len;
            end if;

            Value := Codec.Decode (Data);

         end;
      end Parse_String;

      -----------------------
      -- Parse_String_List --
      -----------------------

      procedure Parse_String_List
        (Value : out League.String_Vectors.Universal_String_Vector)
      is
         Text : League.Strings.Universal_String;
      begin
         Expect (Character'Pos ('l'));

         while Buffer (Next) /= Character'Pos ('e') loop
            Parse_String (Text);
            Value.Append (Text);
         end loop;

         Expect (Character'Pos ('e'));
      end Parse_String_List;

      ---------------------
      -- Skip_Dictionary --
      ---------------------

      procedure Skip_Dictionary is
         Key : League.Strings.Universal_String;
      begin
         Expect (Character'Pos ('d'));

         while Buffer (Next) /= Character'Pos ('e') loop
            Parse_String (Key);
            Skip_Value;
         end loop;

         Expect (Character'Pos ('e'));

      end Skip_Dictionary;

      --------------
      -- Skip_Int --
      --------------

      procedure Skip_Int is
      begin
         Expect (Character'Pos ('i'));

         if Buffer (Next) = Character'Pos ('-') then
            Expect (Buffer (Next));
         end if;

         while Buffer (Next) in Digit loop
            Expect (Buffer (Next));
         end loop;

         Expect (Character'Pos ('e'));
      end Skip_Int;

      ---------------
      -- Skip_List --
      ---------------

      procedure Skip_List is
      begin
         Expect (Character'Pos ('l'));

         while Buffer (Next) /= Character'Pos ('e') loop
            Skip_Value;
         end loop;

         Expect (Character'Pos ('e'));
      end Skip_List;

      -----------------
      -- Skip_String --
      -----------------

      procedure Skip_String is
         Len : Ada.Streams.Stream_Element_Count := 0;
      begin
         while Buffer (Next) in Digit loop
            Len := Len * 10
              + Ada.Streams.Stream_Element_Count (Buffer (Next))
              - Character'Pos ('0');

            Expect (Buffer (Next));
         end loop;

         declare
            To   : Ada.Streams.Stream_Element_Count := 0;
         begin
            Expect (Character'Pos (':'));

            while Last - Next + 1 <= Len loop
               To  := To  +  Last - Next + 1;
               Len := Len - (Last - Next + 1);
               Read_Buffer;
            end loop;

            if Len > 0 then
               Next := Next + Len;
            end if;

         end;
      end Skip_String;

      ----------------
      -- Skip_Value --
      ----------------

      procedure Skip_Value is
      begin
         case Buffer (Next) is
            when Character'Pos ('l') =>
               Skip_List;
            when Digit =>
               Skip_String;
            when Character'Pos ('i') =>
               Skip_Int;
            when Character'Pos ('d') =>
               Skip_Dictionary;
            when others =>
               raise Constraint_Error with Error;
         end case;
      end Skip_Value;

   begin
      Ada.Streams.Stream_IO.Open
        (Input, Ada.Streams.Stream_IO.In_File, File_Name.To_UTF_8_String);
      Ada.Streams.Stream_IO.Read (Input, Buffer, Last);

      Free (Self.Data);
      Self.Data := new Metainfo'(Parse_Top_Dictionary);
      Ada.Streams.Stream_IO.Close (Input);
   end Read;

end Torrent.Metainfo_Files;
