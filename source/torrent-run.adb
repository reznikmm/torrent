--  Copyright (c) 2019-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers;
with Ada.Directories;
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.String_Vectors;
with League.Strings;

with Torrent.Contexts;
with Torrent.Metainfo_Files;
with Torrent.Connections;

procedure Torrent.Run is
   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
         renames League.Strings.To_Universal_String;

   procedure Parse_Command_Line;
   procedure Print_Help;

   procedure Increment_Total (Ignore : Ada.Directories.Directory_Entry_Type);

   procedure Each_Torrents
     (Dir  : League.Strings.Universal_String;
      Proc : not null access procedure
        (Item : Ada.Directories.Directory_Entry_Type));

   Cmd : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;

   Out_Option  : constant Wide_Wide_String := "--output=";
   Dir_Option  : constant Wide_Wide_String := "--torrent-dir=";
   Port_Option : constant Wide_Wide_String := "--port=";
   Help_Option : constant Wide_Wide_String := "--help";

   Port        : Positive := 33411;
   Path        : League.Strings.Universal_String := +"result";
   Input_Path  : League.Strings.Universal_String := +"torrents";
   Total : Ada.Containers.Count_Type := 0;

   ---------------------
   -- Increment_Total --
   ---------------------

   procedure Increment_Total (Ignore : Ada.Directories.Directory_Entry_Type) is
      use type Ada.Containers.Count_Type;
   begin
      Total := Total + 1;
   end Increment_Total;

   -------------------
   -- Each_Torrents --
   -------------------

   procedure Each_Torrents
     (Dir  : League.Strings.Universal_String;
      Proc : not null access procedure
        (Item : Ada.Directories.Directory_Entry_Type))
   is
   begin
      Ada.Directories.Search
        (Directory => Dir.To_UTF_8_String,
         Pattern   => "*.torrent",
         Filter    => (Ada.Directories.Ordinary_File => True, others => False),
         Process   => Proc);
   end Each_Torrents;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      Arg    : League.Strings.Universal_String;
   begin
      for J in 1 .. Cmd.Length loop
         Arg := Cmd.Element (J);

         if Arg.Starts_With (Port_Option) then
            Port := Positive'Wide_Wide_Value
              (Arg.Tail_From (Port_Option'Length + 1).To_Wide_Wide_String);
         elsif Arg.Starts_With (Out_Option) then
            Path := Arg.Tail_From (Out_Option'Length + 1);
         elsif Arg.Starts_With (Dir_Option) then
            Input_Path := Arg.Tail_From (Dir_Option'Length + 1);
         elsif Arg.Starts_With (Help_Option) then
            Print_Help;
         end if;
      end loop;
   end Parse_Command_Line;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Usage: torrent-run <options>");
      Ada.Wide_Wide_Text_IO.Put_Line ("Options are");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("  " & Port_Option & "int - a port to listen");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("  " & Out_Option & "path - a directory to save downloaded files");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("  " & Dir_Option & "path - a directory with torrent files");
   end Print_Help;

begin
   if Cmd.Is_Empty then
      Print_Help;
      return;
   end if;

   Parse_Command_Line;
   Each_Torrents (Input_Path, Increment_Total'Access);

   declare
      procedure Add (Item : Ada.Directories.Directory_Entry_Type);

      Recycle : aliased Torrent.Connections.Queues.Queue;

      Context : Torrent.Contexts.Context
        (Capacity => Total,
         Port     => Port,
         Recycle  => Recycle'Unchecked_Access);

      ---------
      -- Add --
      ---------

      procedure Add (Item : Ada.Directories.Directory_Entry_Type) is
         Meta : constant Torrent.Metainfo_Files.Metainfo_File_Access :=
           new Torrent.Metainfo_Files.Metainfo_File;
      begin
         Meta.Read
           (League.Strings.From_UTF_8_String
              (Ada.Directories.Full_Name (Item)));

         Context.Add_Metainfo_File (Meta);
      end Add;
   begin
      Context.Initialize (Path);
      Each_Torrents (Input_Path, Add'Access);
      Context.Start;
   end;
end Torrent.Run;
