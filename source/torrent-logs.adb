--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Text_IO;

package body Torrent.Logs is

   Output : Ada.Text_IO.File_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Output : League.Strings.Universal_String) is
   begin
      Ada.Text_IO.Create (Torrent.Logs.Output, Name => Output.To_UTF_8_String);
      Enabled := True;
   end Initialize;

   -----------
   -- Print --
   -----------

   procedure Print (Text : String) is
   begin
      Ada.Text_IO.Put_Line (Output, Text);
   end Print;

end Torrent.Logs;
