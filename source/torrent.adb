--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Torrent is

   -----------
   -- Image --
   -----------

   function Image (Value : SHA1) return SHA1_Image is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      Hex_Digit : constant array (Ada.Streams.Stream_Element range 0 .. 15)
        of Wide_Wide_Character := ("0123456789ABCDEF");
   begin
      return S : SHA1_Image do
         for J in Value'Range loop
            declare
               Index : constant Natural := 1 + Natural (J - Value'First) * 2;
            begin
               S (Index)     := Hex_Digit (Value (J) / 16);
               S (Index + 1) := Hex_Digit (Value (J) mod 16);
            end;
         end loop;
      end return;
   end Image;

end Torrent;
