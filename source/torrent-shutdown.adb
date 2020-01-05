--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Torrent.Shutdown is

   protected body Signal is

      entry Wait_SIGINT when SIGINT_Triggered is
      begin
         SIGINT_Triggered := False;
      end Wait_SIGINT;

      procedure Handle is
      begin
         SIGINT_Triggered := True;
      end Handle;

   end Signal;

end Torrent.Shutdown;
