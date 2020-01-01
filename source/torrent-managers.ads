--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

limited with Torrent.Contexts;
with Torrent.Connections;

package Torrent.Managers is

   task type Manager
     (Context : not null access Torrent.Contexts.Context;
      Recycle : not null access
        Torrent.Connections.Queue_Interfaces.Queue'Class)
   is
      entry Connected (Value : not null Torrent.Connections.Connection_Access);
      entry Complete;
   end Manager;

end Torrent.Managers;
