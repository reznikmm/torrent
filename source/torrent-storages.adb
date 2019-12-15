--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Directories;

package body Torrent.Storages is

   use type Ada.Streams.Stream_Element_Count;

   protected body Storage is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Path : League.Strings.Universal_String)
      is
         Offset : Ada.Streams.Stream_Element_Count := 0;
      begin
         Root_Path := Path;

         for J in 1 .. Meta.File_Count loop
            declare
               use type League.Strings.Universal_String;

               Name : constant League.Strings.Universal_String :=
                 Path & "/" & Meta.File_Path (J).Join ("/");
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

            if Meta.File_Length (J) > 0 then
               Files.Insert (Offset, J);
               Offset := Offset + Meta.File_Length (J);
            end if;
         end loop;
      end Initialize;

      ----------
      -- Read --
      ----------

      entry Read
        (Offset : Ada.Streams.Stream_Element_Count;
         Data   : out Ada.Streams.Stream_Element_Array)
        when Reading
      is
         use type League.Strings.Universal_String;

         From : Ada.Streams.Stream_Element_Count := Data'First;
         Last : Ada.Streams.Stream_Element_Count;

         Cursor : File_Index_Maps.Cursor := Files.Floor (Offset);
         --  The last node whose key is not greater (i.e. less or equal).
         --  The first key in map is 0, so we expect Floor fine some item.

         Skip   : Ada.Streams.Stream_Element_Count :=  --  Offset inside a file
           Offset - File_Index_Maps.Key (Cursor);

         File   : Positive := File_Index_Maps.Element (Cursor);

         File_Length : Ada.Streams.Stream_Element_Count :=
           Meta.File_Length (File);

         Name  : constant League.Strings.Universal_String :=
           Root_Path & "/" & Meta.File_Path (File).Join ('/');

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

            if Read_Cache.Name /= Name then
               if Ada.Streams.Stream_IO.Is_Open (Read_Cache.Input) then
                  Ada.Streams.Stream_IO.Close (Read_Cache.Input);
               end if;

               Ada.Streams.Stream_IO.Open
                 (Read_Cache.Input,
                  Ada.Streams.Stream_IO.In_File,
                  Name.To_UTF_8_String,
                  Form => "shared=no");

               Read_Cache.Name := Name;
            end if;

            declare
               use type Ada.Streams.Stream_IO.Count;

               Done : Ada.Streams.Stream_Element_Offset;
            begin
               if Ada.Streams.Stream_IO.Size (Read_Cache.Input) >=
                 Ada.Streams.Stream_IO.Count (Skip + Data'Length)
               then
                  Ada.Streams.Stream_IO.Read
                    (File => Read_Cache.Input,
                     Item => Data (From .. Last),
                     Last => Done,
                     From => Ada.Streams.Stream_IO.Count (Skip + 1));

                  pragma Assert (Done = Last);

               else
                  Data := (others => 0);
               end if;

            end;

            exit when Last >= Data'Last;

            File_Index_Maps.Next (Cursor);
            Skip := 0;
            File := File_Index_Maps.Element (Cursor);
            File_Length := Meta.File_Length (File);
            From := Last + 1;
         end loop;
      end Read;

      -------------------
      -- Start_Reading --
      -------------------

      entry Start_Reading when not Reading is
      begin
         Reading := True;
      end Start_Reading;

      ------------------
      -- Stop_Reading --
      ------------------

      entry Stop_Reading when Reading is
      begin
         Reading := False;
      end Stop_Reading;

      -----------
      -- Write --
      -----------

      entry Write
        (Offset : Ada.Streams.Stream_Element_Count;
         Data   : Ada.Streams.Stream_Element_Array)
        when not Reading
      is
         Cursor      : File_Index_Maps.Cursor := Files.Floor (Offset);
         --  The last node whose key is not greater (i.e. less or equal).
         --  The first key in map is 0, so we expect Floor fine some item.
         Skip        : Ada.Streams.Stream_Element_Count :=  --  File offset
           Offset - File_Index_Maps.Key (Cursor);
         File        : Positive := File_Index_Maps.Element (Cursor);
         File_Length : Ada.Streams.Stream_Element_Count :=
           Meta.File_Length (File);
         From        : Ada.Streams.Stream_Element_Count := Data'First;
         Last        : Ada.Streams.Stream_Element_Count;
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
               Name   : constant League.Strings.Universal_String :=
                 Root_Path & "/" & Meta.File_Path (File).Join ('/');
            begin
               if Read_Cache.Name = Name then
                  Read_Cache.Name.Clear;
                  Ada.Streams.Stream_IO.Close (Read_Cache.Input);
               end if;

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
            File_Length := Meta.File_Length (File);
            From := Last + 1;
         end loop;
      end Write;

   end Storage;

end Torrent.Storages;
