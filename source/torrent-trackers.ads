--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.IRIs;
with League.Strings;

package Torrent.Trackers is

   type Announcement_Kind is
     (Started, Completed, Stopped, Regular);

   function Event_URL
     (Tracker     : League.IRIs.IRI;
      Info_Hash   : SHA1;
      Peer_Id     : SHA1;
      Port        : Positive;
      Uploaded    : Ada.Streams.Stream_Element_Count;
      Downloaded  : Ada.Streams.Stream_Element_Count;
      Left        : Ada.Streams.Stream_Element_Count;
      Event       : Announcement_Kind) return League.IRIs.IRI;
   --  Construct an URL to request a tracker.

   type Response (<>) is tagged private;

   function Parse (Data : Ada.Streams.Stream_Element_Array) return Response;
   --  Decode tracker's response. Constraint_Error is raised if it fails.

   function Is_Failure (Self : Response'Class) return Boolean;
   --  If the query failed.

   function Failure_Reason
     (Self : Response'Class) return League.Strings.Universal_String;
   --  A human readable string which explains why the query failed.

   function Interval (Self : Response'Class) return Duration;
   --  The number of seconds the downloader should wait between regular
   --  rerequests.

   function Peer_Count (Self : Response'Class) return Natural;
   --  Length of peer lists.

   function Peer_Id
     (Self  : Response'Class;
      Index : Positive) return SHA1;
   --  The peer's self-selected ID

   function Peer_Address
     (Self  : Response'Class;
      Index : Positive) return League.Strings.Universal_String;
   --  The peer's IP address or DNS name.

   function Peer_Port
     (Self  : Response'Class;
      Index : Positive) return Natural;
   --  The peer's port number.

private

   type Peer is record
      Id      : SHA1;
      Address : League.Strings.Universal_String;
      Port    : Natural;
   end record;

   type Peer_Array is array (Positive range <>) of Peer;

   type Response (Peer_Count : Natural) is tagged record
      Is_Failure     : Boolean;
      Failure_Reason : League.Strings.Universal_String;
      Interval       : Duration;
      Peers          : Peer_Array (1 .. Peer_Count);
   end record;

end Torrent.Trackers;
