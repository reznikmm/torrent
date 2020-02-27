--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Containers;
with Ada.Containers.Bounded_Ordered_Maps;

with GNAT.Sockets;

with League.Strings;

with Torrent.Metainfo_Files;
with Torrent.Downloaders;
with Torrent.Initiators;
with Torrent.Managers;
with Torrent.Connections;

package Torrent.Contexts is

   type Context
     (Capacity : Ada.Containers.Count_Type;
      Port     : Natural;
      Recycle  : not null access
        Torrent.Connections.Queue_Interfaces.Queue'Class)
          is tagged limited private;

   procedure Initialize
     (Self : in out Context'Class;
      Id   : Torrent.SHA1;
      Path : League.Strings.Universal_String);

   procedure Add_Metainfo_File
     (Self : in out Context'Class;
      File : not null Torrent.Metainfo_Files.Metainfo_File_Access);

   procedure Start
     (Self        : in out Context'Class;
      Next_Update : out Ada.Calendar.Time);

   procedure Update
     (Self        : in out Context'Class;
      Next_Update : out Ada.Calendar.Time);

   procedure Stop (Self : in out Context'Class);

   function Find_Download
     (Self : Context'Class;
      Hash : SHA1) return Torrent.Downloaders.Downloader_Access;

   --  For interlnal usage

   procedure Connected
     (Self  : Context'Class;
      Value : Torrent.Connections.Connection_Access);

   procedure Connect
     (Self    : Context'Class;
      Job     : Torrent.Downloaders.Downloader_Access;
      Address : GNAT.Sockets.Sock_Addr_Type);

private

   type Downloader_Access is access all Torrent.Downloaders.Downloader'Class;

   package Downloader_Maps is new Ada.Containers.Bounded_Ordered_Maps
     (Key_Type     => SHA1,
      Element_Type => Downloader_Access,
      "<"          => Ada.Streams."<",
      "="          => "=");

   type Context
     (Capacity : Ada.Containers.Count_Type;
      Port     : Natural;
      Recycle  : not null access
        Torrent.Connections.Queue_Interfaces.Queue'Class) is tagged limited
   record
      Path        : League.Strings.Universal_String;
      Downloaders : Downloader_Maps.Map (Capacity);
      Last        : Natural := 0;  --  Last item in Downloaders array
      Peer_Id     : SHA1;
      Initiator   : Torrent.Initiators.Initiator
                      (Port, Context'Unchecked_Access, Recycle);
      Manager     : Torrent.Managers.Manager
                      (Context'Unchecked_Access, Recycle);
   end record;

end Torrent.Contexts;
