--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers;
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.String_Vectors;
with League.Strings;

with Torrent.Downloaders;
with Torrent.Metainfo_Files;

procedure Torrent.Run is
   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
         renames League.Strings.To_Universal_String;

   Cmd : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Meta : aliased Torrent.Metainfo_Files.Metainfo_File;
   Path : League.String_Vectors.Universal_String_Vector;
begin
   Path.Append (+"result");
   Meta.Read (Cmd (1));

   declare
      Count : constant Ada.Containers.Count_Type :=
        Ada.Containers.Count_Type (Meta.File_Count);
      DL    : Torrent.Downloaders.Downloader
        (Meta'Unchecked_Access, Count, Meta.Piece_Count);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (Meta.Announce.To_Universal_String.To_Wide_Wide_String);

      DL.Start
        (Port => 12345,
         Path => Path);
   end;
end Torrent.Run;
