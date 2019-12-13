--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Finalization;

with GNAT.Sockets;

with League.Stream_Element_Vectors;

with Torrent.Metainfo_Files;
with Torrent.Storages;

package Torrent.Connections is

   type Connection is tagged;
   type Connection_Access is access all Connection'Class;

   type Interval is record
      From : Piece_Offset;  --  Starts from zero
      To   : Piece_Offset;
   end record;

   package Interval_Vectors is new Ada.Containers.Vectors
     (Positive, Interval);

   type Piece_State is record
      Piece     : Natural;
      Intervals : Interval_Vectors.Vector;
   end record;

   type Connection_State_Listener is synchronized interface;

   type Connection_State_Listener_Access is
     access all Connection_State_Listener'Class
       with Storage_Size => 0;

   not overriding function We_Are_Intrested
     (Self : Connection_State_Listener;
      Map  : Boolean_Array) return Boolean is abstract;

   not overriding procedure Reserve_Intervals
     (Self       : in out Connection_State_Listener;
      Connection : Connection_Access;
      Map        : Boolean_Array;
      Value      : out Piece_State) is abstract;
   --  If connection peer inform, that new piece is ready recalculate
   --  if we are intrested and assign new piece to download.

   type Connection
     (Meta        : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      Storage     : not null Torrent.Storages.Storage_Access;
      Piece_Count : Positive) is tagged limited private;

   procedure Initialize
     (Self     : in out Connection'Class;
      My_Id    : SHA1;
      Peer     : GNAT.Sockets.Sock_Addr_Type;
      Listener : Connection_State_Listener_Access);

   function Connected (Self : Connection'Class) return Boolean;
   function Intrested (Self : Connection'Class) return Boolean;
   function Speed (Unused : Connection'Class) return Natural is (0);

   procedure Set_Choked
     (Self  : in out Connection'Class;
      Value : Boolean);

   procedure Serve
     (Self : in out Connection'Class;
      Time : Duration);

private

   type Request is record
      Index  : Positive;
      Offset : Natural;
      Length : Positive;
   end record;

   type Request_Queue is array (1 .. 4) of Request;

   type Connection
     (Meta        : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      Storage     : not null Torrent.Storages.Storage_Access;
      Piece_Count : Positive) is limited
     new Ada.Finalization.Limited_Controlled with
   record
      Peer           : GNAT.Sockets.Sock_Addr_Type;
      Socket         : GNAT.Sockets.Socket_Type;
      Sent_Handshake : Boolean;
      Got_Handshake  : Boolean;
      Closed         : Boolean;
      We_Choked      : Boolean;
      He_Choked      : Boolean;
      Choked_Sent    : Boolean;  --  If He_Choked is in action
      He_Intrested   : Boolean;
      We_Intrested   : Boolean;
      Current_Piece  : Piece_State;
      My_Peer_Id     : SHA1;
      Unparsed       : League.Stream_Element_Vectors.Stream_Element_Vector :=
        League.Stream_Element_Vectors.Empty_Stream_Element_Vector;
      Requests       : Request_Queue;
      Last_Request   : Natural;
      Listener       : Connection_State_Listener_Access;
      Piece_Map      : Boolean_Array (1 .. Piece_Count);
   end record;
end Torrent.Connections;
