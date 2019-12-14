--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Directories;
with Ada.Streams.Stream_IO;

package body Torrent.Storages is

   use type Ada.Streams.Stream_Element_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Storage'Class;
      Path : League.Strings.Universal_String)
   is
      Offset : Ada.Streams.Stream_Element_Count := 0;
   begin
      Self.Path := Path;

      for J in 1 .. Self.Meta.File_Count loop
         declare
            use type League.Strings.Universal_String;

            Name : constant League.Strings.Universal_String :=
              Path & "/" & Self.Meta.File_Path (J).Join ("/");
            File : constant String := Name.To_UTF_8_String;
            Dir  : constant String :=
              Ada.Directories.Containing_Directory (File);
         begin
            if not Ada.Directories.Exists (Dir) then
               Ada.Directories.Create_Path (Dir);
            end if;

            if not Ada.Directories.Exists (File) then
               declare
                  Dummy : Ada.Streams.Stream_IO.File_Type;
               begin
                  Ada.Streams.Stream_IO.Create
                    (Dummy, Name => File);
                  Ada.Streams.Stream_IO.Close (Dummy);
               end;
            end if;

         end;

         if Self.Meta.File_Length (J) > 0 then
            Self.Files.Insert (Offset, J);
            Offset := Offset + Self.Meta.File_Length (J);
         end if;
      end loop;
   end Initialize;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self   : Storage'Class;
      Offset : Ada.Streams.Stream_Element_Count;
      Data   : out Ada.Streams.Stream_Element_Array)
   is
      Cursor : File_Index_Maps.Cursor := Self.Files.Floor (Offset);
      --  The last node whose key is not greater (i.e. less or equal).
      --  The first key in map is 0, so we expect Floor fine some item.
      Skip : Ada.Streams.Stream_Element_Count :=  --  Offset inside a file
        Offset - File_Index_Maps.Key (Cursor);
      File : Positive := File_Index_Maps.Element (Cursor);
      File_Length : Ada.Streams.Stream_Element_Count :=
        Self.Meta.File_Length (File);
      From : Ada.Streams.Stream_Element_Count := Data'First;
      Last : Ada.Streams.Stream_Element_Count;
   begin
      if Skip >= File_Length then
         return;  --  Offset is greater then torrent size.
      end if;

      loop
         if Skip + Data'Last - From + 1 > File_Length then
            Last := From + File_Length - Skip - 1;
         else
            Last := Data'Last;
         end if;

         declare
            use type League.Strings.Universal_String;

            Input : Ada.Streams.Stream_IO.File_Type;
            Name  : constant League.Strings.Universal_String :=
              Self.Path & "/" & Self.Meta.File_Path (File).Join ('/');
            Done : Ada.Streams.Stream_Element_Offset;
         begin
            Ada.Streams.Stream_IO.Open
              (Input,
               Ada.Streams.Stream_IO.In_File,
               Name.To_UTF_8_String,
               Form => "shared=no");

            Ada.Streams.Stream_IO.Read
              (File => Input,
               Item => Data (From .. Last),
               Last => Done,
               From => Ada.Streams.Stream_IO.Count (Skip + 1));

            pragma Assert (Done = Last);

            Ada.Streams.Stream_IO.Close (Input);
         end;

         exit when Last >= Data'Last;

         File_Index_Maps.Next (Cursor);
         Skip := 0;
         File := File_Index_Maps.Element (Cursor);
         File_Length := Self.Meta.File_Length (File);
         From := Last + 1;
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self   : Storage'Class;
      Offset : Ada.Streams.Stream_Element_Count;
      Data   : Ada.Streams.Stream_Element_Array)
   is
      Cursor : File_Index_Maps.Cursor := Self.Files.Floor (Offset);
      --  The last node whose key is not greater (i.e. less or equal).
      --  The first key in map is 0, so we expect Floor fine some item.
      Skip : Ada.Streams.Stream_Element_Count :=  --  Offset inside a file
        Offset - File_Index_Maps.Key (Cursor);
      File : Positive := File_Index_Maps.Element (Cursor);
      File_Length : Ada.Streams.Stream_Element_Count :=
        Self.Meta.File_Length (File);
      From : Ada.Streams.Stream_Element_Count := Data'First;
      Last : Ada.Streams.Stream_Element_Count;
   begin
      if Skip >= File_Length then
         return;  --  Offset is greater then torrent size.
      end if;

      loop
         if Skip + Data'Last - From + 1 > File_Length then
            Last := From + File_Length - Skip - 1;
         else
            Last := Data'Last;
         end if;

         declare
            use type League.Strings.Universal_String;

            Output : Ada.Streams.Stream_IO.File_Type;
            Name  : constant League.Strings.Universal_String :=
              Self.Path & "/" & Self.Meta.File_Path (File).Join ('/');
         begin
            Ada.Streams.Stream_IO.Open
              (Output,
               Ada.Streams.Stream_IO.Out_File,
               Name.To_UTF_8_String,
               Form => "shared=no");

            Ada.Streams.Stream_IO.Write
              (File => Output,
               Item => Data (From .. Last),
               To   => Ada.Streams.Stream_IO.Count (Skip + 1));

            Ada.Streams.Stream_IO.Close (Output);
         end;

         exit when Last >= Data'Last;

         File_Index_Maps.Next (Cursor);
         Skip := 0;
         File := File_Index_Maps.Element (Cursor);
         File_Length := Self.Meta.File_Length (File);
         From := Last + 1;
      end loop;
   end Write;

end Torrent.Storages;
