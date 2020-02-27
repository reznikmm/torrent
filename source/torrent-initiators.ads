--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with GNAT.Sockets;

with Torrent.Connections;
with Torrent.Downloaders;
limited with Torrent.Contexts;

package Torrent.Initiators is

   task type Initiator
     (Port    : Natural;
      Context : not null access Torrent.Contexts.Context;
      Recycle : not null access
        Torrent.Connections.Queue_Interfaces.Queue'Class)
   is
      entry Connect
        (Downloader : not null Torrent.Downloaders.Downloader_Access;
         Address    : GNAT.Sockets.Sock_Addr_Type);
      entry Stop;
   end Initiator;

end Torrent.Initiators;
