--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Bounded_Ordered_Maps;
with Ada.Streams.Stream_IO;

with League.Strings;

with Torrent.Metainfo_Files;

package Torrent.Storages is

   package File_Index_Maps is new Ada.Containers.Bounded_Ordered_Maps
     (Key_Type     => Ada.Streams.Stream_Element_Count,
      Element_Type => Positive,
      "<"          => Ada.Streams."<",
      "="          => "=");

   type Read_Cached_File is record
      Name  : League.Strings.Universal_String;
      Input : Ada.Streams.Stream_IO.File_Type;
   end record;

   protected type Storage
     (Meta       : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      File_Count : Ada.Containers.Count_Type) is

      procedure Initialize (Path : League.Strings.Universal_String);

      entry Start_Reading;
      entry Stop_Reading;
      --  Should be called before and after Read

      entry Read
        (Offset : Ada.Streams.Stream_Element_Count;
         Data   : out Ada.Streams.Stream_Element_Array);

      entry Write
        (Offset : Ada.Streams.Stream_Element_Count;
         Data   : Ada.Streams.Stream_Element_Array);

   private

      Reading    : Boolean := False;
      Files      : File_Index_Maps.Map (File_Count);
      Root_Path  : League.Strings.Universal_String;
      Read_Cache : Read_Cached_File;
   end Storage;

   type Storage_Access is access all Storage;

end Torrent.Storages;
