--  Copyright (c) 2019-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;

package Torrent is

   pragma Pure;

   subtype SHA1 is Ada.Streams.Stream_Element_Array (1 .. 20);
   subtype SHA1_Image is Wide_Wide_String (1 .. 40);

   function Image (Value : SHA1) return SHA1_Image;

   subtype Piece_Offset is Ada.Streams.Stream_Element_Count;

   type Piece_Count is new Natural;

   subtype Piece_Index is Piece_Count range 1 .. Piece_Count'Last;

   type Boolean_Array is array (Piece_Index range <>) of Boolean
     with Pack;

   Max_Interval_Size : constant := 16 * 1024;
   Seed_Time         : constant Duration := 10.0;

end Torrent;
