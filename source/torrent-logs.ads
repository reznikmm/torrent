--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

package Torrent.Logs is

   Enabled : Boolean := False;

   procedure Initialize (Output : League.Strings.Universal_String);

   procedure Print (Text : String);

end Torrent.Logs;
