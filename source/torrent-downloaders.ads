--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Finalization;

with GNAT.Sockets;

with League.IRIs;
with League.Stream_Element_Vectors;

with Torrent.Metainfo_Files;
with Torrent.Trackers;
with League.String_Vectors;

package Torrent.Downloaders is

   type Downloader
     (Meta : not null access Torrent.Metainfo_Files.Metainfo_File) is
        tagged limited private;
   --  The downloader tracks one torrent file and all connections
   --  related to it.

   procedure Initialize
     (Self : in out Downloader'Class;
      Port : Positive;
      Path : League.String_Vectors.Universal_String_Vector);

   type Request_Kind is
     (No_Request,
      Tracker_Request,
      Peer_Request);

   subtype Some_Request is Request_Kind range Tracker_Request .. Peer_Request;

   type Request (Kind : Request_Kind := Request_Kind'First) is record
      case Kind is
         when No_Request =>
            null;
         when Tracker_Request =>
            URL : League.IRIs.IRI;
         when Peer_Request =>
            Peer    : GNAT.Sockets.Sock_Addr_Type;
            Request : League.Stream_Element_Vectors.Stream_Element_Vector;
      end case;
   end record;

   type Response (Kind : Some_Request := Peer_Request) is record
      Message : League.Stream_Element_Vectors.Stream_Element_Vector;

      case Kind is
         when Tracker_Request =>
            Success : Boolean;
         when Peer_Request =>
            Peer    : GNAT.Sockets.Sock_Addr_Type;
            Closed  : Boolean;
      end case;
   end record;

   procedure Get_Request
     (Self  : in out Downloader;
      Value : out Request);

   procedure New_Response
     (Self  : in out Downloader;
      Value : Response);

private

   type Interval is record
      From : Natural;
      To   : Positive;
   end record;

   package Interval_Vectors is new Ada.Containers.Vectors
     (Positive, Interval);

   type Piece_State is record
      Intervals : Interval_Vectors.Vector;
   end record;

   package Piece_State_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Piece_State);

   type Boolean_Array is array (Positive range <>) of Boolean
     with Pack;

   type Boolean_Array_Access is access all Boolean_Array;

   type Server_State is record
      Peer         : GNAT.Sockets.Sock_Addr_Type;
      Handshake    : Boolean;
      Closed       : Boolean;
      We_Choked    : Boolean;
      He_Intrested : Boolean;
      First        : Natural;  --  Index in Piece_Map
      Unparsed     : League.Stream_Element_Vectors.Stream_Element_Vector :=
        League.Stream_Element_Vectors.Empty_Stream_Element_Vector;
   end record;

   function Hash
     (Value : GNAT.Sockets.Sock_Addr_Type) return Ada.Containers.Hash_Type;

   package Server_State_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNAT.Sockets.Sock_Addr_Type,
      Element_Type    => Server_State,
      Hash            => Hash,
      Equivalent_Keys => GNAT.Sockets."=");

   type Tracker_Response_Access is access Torrent.Trackers.Response;

   type Command_Kind is (New_Request);

   type Command (Kind : Command_Kind := New_Request) is record
      case Kind is
         when New_Request =>
            Piece : Positive;
            Offset : Natural;
            Length : Positive;
      end case;
   end record;

   package Command_Vectors is new Ada.Containers.Vectors
     (Positive, Command);

   type Downloader
     (Meta : not null access Torrent.Metainfo_Files.Metainfo_File) is
     new Ada.Finalization.Limited_Controlled with
      record
         Path             : League.String_Vectors.Universal_String_Vector;
         Tracker_Response : Tracker_Response_Access;
         Piece_Map        : Boolean_Array_Access;
         Last             : Positive;
         Peer_Id          : SHA1;
         Port             : Positive;
         Uploaded         : Ada.Streams.Stream_Element_Count;
         Downloaded       : Ada.Streams.Stream_Element_Count;
         Left             : Ada.Streams.Stream_Element_Count;
         Last_Piece_Size  : Positive;
         Pieces           : Piece_State_Vectors.Vector;
         Servers          : Server_State_Maps.Map;
         Commands         : Command_Vectors.Vector;
      end record;

   procedure Handshake
     (Self   : in out Downloader;
      Server : in out Server_State;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector);

   procedure Message
     (Self   : in out Downloader;
      Server : in out Server_State;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector);

   procedure Message
     (Self   : in out Downloader;
      Server : in out Server_State;
      Value  : Ada.Streams.Stream_Element_Array);

end Torrent.Downloaders;
