with Torrent.Connections;
with Torrent.Downloaders;

package Torrent.Initiators is

   task Initiator is
      entry Connect
        (Downloader : not null Torrent.Downloaders.Downloader_Access;
         Value      : not null Torrent.Connections.Connection_Access);
      entry Stop;
   end Initiator;

end Torrent.Initiators;
