--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Unchecked_Conversion;

package Torrent.Handshakes is

   use type Ada.Streams.Stream_Element_Count;

   Header : constant String := "BitTorrent protocol";

   subtype Handshake_Image is Ada.Streams.Stream_Element_Array
     (1 .. 1 + Header'Length + 8 + 2 * SHA1'Length);

   type Handshake_Type is record
      Length    : Ada.Streams.Stream_Element := Header'Length;
      Head      : String (Header'Range) := Header;
      Zeros     : Ada.Streams.Stream_Element_Array (1 .. 8) := (others => 0);
      Info_Hash : SHA1;
      Peer_Id   : SHA1;
   end record
     with Pack, Size => 8 * (1 + Header'Length + 8 + 2 * SHA1'Length);

   function "+" is new Ada.Unchecked_Conversion
     (Handshake_Type, Handshake_Image);

   function "-" is new Ada.Unchecked_Conversion
     (Handshake_Image, Handshake_Type);

end Torrent.Handshakes;
