--  Copyright (c) 2019-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces;
with System;

with Torrent.Trackers;

package Torrent.UDP_Tracker_Protocol is

   type Connect_Request is record
      Protocol_Id    : Interfaces.Unsigned_64 := 16#417_27_10_1980#;
      Action         : Interfaces.Unsigned_32;
      Transaction_Id : Interfaces.Unsigned_32;
   end record;

   for Connect_Request'Bit_Order use System.High_Order_First;
   for Connect_Request'Scalar_Storage_Order use System.High_Order_First;

   for Connect_Request use record
      Protocol_Id    at 0 range 0 .. 63;
      Action         at 8 range 0 .. 31;
      Transaction_Id at 12 range 0 .. 31;
   end record;

   subtype Raw_Connect_Request is Ada.Streams.Stream_Element_Array
     (1 .. Connect_Request'Max_Size_In_Storage_Elements);

   function Cast is new Ada.Unchecked_Conversion
     (Connect_Request, Raw_Connect_Request);

   type Connect_Response is record
      Action         : Interfaces.Unsigned_32;
      Transaction_Id : Interfaces.Unsigned_32;
      Connection_Id  : Interfaces.Unsigned_64;
   end record;

   for Connect_Response'Bit_Order use System.High_Order_First;
   for Connect_Response'Scalar_Storage_Order use System.High_Order_First;

   for Connect_Response use record
      Action         at 0 range 0 .. 31;
      Transaction_Id at 4 range 0 .. 31;
      Connection_Id  at 8 range 0 .. 63;
   end record;

   function Cast is new Ada.Unchecked_Conversion
     (Raw_Connect_Request, Connect_Response);

   subtype IPV4_Address is Ada.Streams.Stream_Element_Array (1 .. 4);

   pragma Warnings (Off, "scalar storage order specified for");

   type Announce_Request is record
      Connection_Id  : Interfaces.Unsigned_64;
      Action         : Interfaces.Unsigned_32;
      Transaction_Id : Interfaces.Unsigned_32;
      Info_Hash      : SHA1;
      Peer_Id        : SHA1;
      Downloaded     : Interfaces.Unsigned_64;
      Left           : Interfaces.Unsigned_64;
      Uploaded       : Interfaces.Unsigned_64;
      Event          : Interfaces.Unsigned_32;
      IP_Address     : IPV4_Address := (others => 0);
      Key            : Interfaces.Unsigned_32;
      Num_Want       : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;
      Port           : Interfaces.Unsigned_16;
   end record;

   for Announce_Request'Bit_Order use System.High_Order_First;
   for Announce_Request'Scalar_Storage_Order use System.High_Order_First;

   for Announce_Request use record
      Connection_Id  at 0 range 0 .. 63;
      Action         at 8 range 0 .. 31;
      Transaction_Id at 12 range 0 .. 31;
      Info_Hash      at 16 range 0 .. 159;
      Peer_Id        at 36 range 0 .. 159;
      Downloaded     at 56 range 0 .. 63;
      Left           at 64 range 0 .. 63;
      Uploaded       at 72 range 0 .. 63;
      Event          at 80 range 0 .. 31;
      IP_Address     at 84 range 0 .. 31;
      Key            at 88 range 0 .. 31;
      Num_Want       at 92 range 0 .. 31;
      Port           at 96 range 0 .. 15;
   end record;

   subtype Raw_Announce_Request is Ada.Streams.Stream_Element_Array
     (1 .. 98);  --  Announce_Request'Max_Size_In_Storage_Elements

   function Cast is new Ada.Unchecked_Conversion
     (Announce_Request, Raw_Announce_Request);

   Cast_Event : constant array (Torrent.Trackers.Announcement_Kind) of
     Interfaces.Unsigned_32 :=
       (Torrent.Trackers.Started   => 2,
        Torrent.Trackers.Completed => 1,
        Torrent.Trackers.Stopped   => 3,
        Torrent.Trackers.Regular   => 0);

   type Announce_Response is record
      Action         : Interfaces.Unsigned_32;
      Transaction_Id : Interfaces.Unsigned_32;
      Interval       : Interfaces.Unsigned_32;
      Leechers       : Interfaces.Unsigned_32;
      Seeders        : Interfaces.Unsigned_32;
   end record;

   for Announce_Response'Bit_Order use System.High_Order_First;
   for Announce_Response'Scalar_Storage_Order use System.High_Order_First;

   for Announce_Response use record
      Action         at 0 range 0 .. 31;
      Transaction_Id at 4 range 0 .. 31;
      Interval       at 8 range 0 .. 31;
      Leechers       at 12 range 0 .. 31;
      Seeders        at 16 range 0 .. 31;
   end record;

   subtype Raw_Announce_Response is Ada.Streams.Stream_Element_Array
     (1 .. Announce_Response'Max_Size_In_Storage_Elements);

   function Cast is new Ada.Unchecked_Conversion
     (Raw_Announce_Response, Announce_Response);

   type Peer_Address is record
      IP_Address : IPV4_Address;
      Port       : Interfaces.Unsigned_16;
   end record;

   for Peer_Address'Bit_Order use System.High_Order_First;
   for Peer_Address'Scalar_Storage_Order use System.High_Order_First;

   for Peer_Address use record
      IP_Address at 0 range 0 .. 31;
      Port       at 4 range 0 .. 15;
   end record;

   subtype Raw_Peer_Address is Ada.Streams.Stream_Element_Array
     (1 .. Peer_Address'Max_Size_In_Storage_Elements);

   function Cast is new Ada.Unchecked_Conversion
     (Raw_Peer_Address, Peer_Address);

end Torrent.UDP_Tracker_Protocol;
