--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
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

end Torrent;
