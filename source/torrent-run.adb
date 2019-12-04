--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.String_Vectors;

with Torrent.Metainfo_Files;

procedure Torrent.Run is
   Cmd : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Meta : constant Torrent.Metainfo_Files.Metainfo_File :=
     Torrent.Metainfo_Files.Read (Cmd (1));
begin
   Ada.Wide_Wide_Text_IO.Put_Line
     (Meta.Announce.To_Universal_String.To_Wide_Wide_String);
end Torrent.Run;
