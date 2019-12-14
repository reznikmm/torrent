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

   procedure Insert
     (List  : in out Interval_Vectors.Vector;
      Value : Interval);

   type Piece_Interval is record
      Piece : Positive;
      Span  : Interval;
   end record;

   subtype Piece_Interval_Count is Natural range 0 .. 8;

   type Piece_Interval_Array is array (Positive range <>) of Piece_Interval;

   type Piece_Intervals (Length : Piece_Interval_Count := 0) is record
      List : Piece_Interval_Array (1 .. Length);
   end record;

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
     (Self  : in out Connection_State_Listener;
      Map   : Boolean_Array;
      Value : out Piece_State) is abstract;

   not overriding procedure Unreserve_Intervals
     (Self : in out Connection_State_Listener;
      Map  : Piece_Interval_Array) is abstract;

   not overriding procedure Interval_Saved
     (Self  : in out Connection_State_Listener;
      Piece : Positive;
      Value : Interval;
      Last  : out Boolean) is abstract;

   not overriding procedure Piece_Completed
     (Self  : in out Connection_State_Listener;
      Piece : Positive;
      Ok    : Boolean) is abstract;

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

   type Natural_Array is array (Positive range <>) of Natural;

   type Sent_Piece_Intervals (Length : Piece_Interval_Count := 0) is record
      Request : Piece_Intervals (Length);
      Expire  : Natural_Array (1 .. Length);
   end record;

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
      Pipelined      : Sent_Piece_Intervals;
      Requests       : Request_Queue;
      Last_Request   : Natural;
      Listener       : Connection_State_Listener_Access;
      Piece_Map      : Boolean_Array (1 .. Piece_Count);
   end record;
end Torrent.Connections;