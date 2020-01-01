--  Copyright (c) 2019-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

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

   Cmd : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Meta : aliased Torrent.Metainfo_Files.Metainfo_File;
   Path : constant League.Strings.Universal_String := +"result";
begin
   Meta.Read (Cmd (1));

   declare
      Recycle : aliased Torrent.Connections.Queues.Queue;
      Context : Torrent.Contexts.Context
        (Capacity => 1, Port => 33411, Recycle => Recycle'Unchecked_Access);

   begin
      Context.Initialize (Path);
      Context.Add_Metainfo_File (Meta'Unchecked_Access);
      Context.Start;
   end;
end Torrent.Run;
