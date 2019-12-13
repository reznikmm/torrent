--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Bounded_Ordered_Maps;
with League.Strings;

with Torrent.Metainfo_Files;

package Torrent.Storages is

   type Storage
     (Meta        : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      File_Count  : Ada.Containers.Count_Type) is tagged limited private;

   type Storage_Access is access all Storage'Class;

   procedure Initialize
     (Self : in out Storage'Class;
      Path : League.Strings.Universal_String);

   procedure Read
     (Self   : Storage'Class;
      Offset : Ada.Streams.Stream_Element_Count;
      Data   : out Ada.Streams.Stream_Element_Array);

   procedure Write
     (Self   : Storage'Class;
      Offset : Ada.Streams.Stream_Element_Count;
      Data   : Ada.Streams.Stream_Element_Array);

private

   package File_Index_Maps is new Ada.Containers.Bounded_Ordered_Maps
     (Key_Type     => Ada.Streams.Stream_Element_Count,
      Element_Type => Positive,
      "<"          => Ada.Streams."<",
      "="          => "=");

   type Storage
     (Meta       : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      File_Count : Ada.Containers.Count_Type) is tagged limited
   record
      Files  : File_Index_Maps.Map (File_Count);
      Path   : League.Strings.Universal_String;
   end record;

end Torrent.Storages;
