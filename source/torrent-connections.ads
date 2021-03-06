--  Copyright (c) 2019-2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Finalization;

with GNAT.Sockets;

with League.Stream_Element_Vectors;

with Torrent.Metainfo_Files;
with Torrent.Storages;
limited with Torrent.Downloaders;

package Torrent.Connections is

   type Connection is tagged;
   type Connection_Access is access all Connection'Class;

   type Connection_Access_Array is
     array (Positive range <>) of Connection_Access;

   package Queue_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces (Connection_Access);

   package Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces);

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
      Piece : Piece_Index;
      Span  : Interval;
   end record;

   subtype Piece_Interval_Count is Natural range 0 .. 8;

   type Piece_Interval_Array is array (Positive range <>) of Piece_Interval;

   type Piece_Intervals (Length : Piece_Interval_Count := 0) is record
      List : Piece_Interval_Array (1 .. Length);
   end record;

   type Piece_State is record
      Piece     : Piece_Count;
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
      Piece : Piece_Index;
      Value : Interval;
      Last  : out Boolean) is abstract;

   not overriding procedure Piece_Completed
     (Self  : in out Connection_State_Listener;
      Piece : Piece_Index;
      Ok    : Boolean) is abstract;

   not overriding procedure Interval_Sent
     (Self : in out Connection_State_Listener;
      Size : Piece_Offset) is abstract;

   type Connection
     (Meta        : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      Downloader  : not null access Torrent.Downloaders.Downloader'Class;
      Storage     : not null Torrent.Storages.Storage_Access;
      Piece_Count : Piece_Index) is tagged limited private;

   procedure Initialize
     (Self     : in out Connection'Class;
      My_Id    : SHA1;
      Peer     : GNAT.Sockets.Sock_Addr_Type;
      Listener : Connection_State_Listener_Access);

   type Piece_Index_Array is array (Piece_Index range <>) of Piece_Index;

   procedure Do_Handshake
     (Self      : in out Connection'Class;
      Socket    : GNAT.Sockets.Socket_Type;
      Completed : Piece_Index_Array;
      Inbound   : Boolean);

   function Peer (Self : Connection'Class) return GNAT.Sockets.Sock_Addr_Type;
   function Connected (Self : Connection'Class) return Boolean;
   function Intrested (Self : Connection'Class) return Boolean;

   function Downloaded (Self : in out Connection'Class) return Piece_Offset;
   --  Return amount of data received since last call.

   function Serve_Needed (Self : Connection'Class) return Boolean;
   --  Connection has some pending messages to send them right now

   procedure Set_Choked
     (Self  : in out Connection'Class;
      Value : Boolean);

   procedure Serve
     (Self  : in out Connection'Class;
      Limit : Ada.Calendar.Time);
   --  Process input messages and seed some data if unchoked. Limit is a time
   --  when it should complete.

   package Connection_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Connection_Access);

   procedure Serve_All
     (Selector : GNAT.Sockets.Selector_Type;
      Vector   : in out Connection_Vectors.Vector;
      Except   : Connection_Access_Array;
      Limit    : Ada.Calendar.Time);

   function Is_Valid_Piece
     (Meta    : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      Storage : in out Torrent.Storages.Storage;
      Piece   : Piece_Index) return Boolean;

private

   package Piece_Interval_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Piece_Interval,
      "="          => "=");

   type Natural_Array is array (Positive range <>) of Natural;

   type Sent_Piece_Intervals (Length : Piece_Interval_Count := 0) is record
      Request : Piece_Intervals (Length);
      Expire  : Natural_Array (1 .. Length);
   end record;

   type Connection
     (Meta        : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      Downloader  : not null access Torrent.Downloaders.Downloader'Class;
      Storage     : not null Torrent.Storages.Storage_Access;
      Piece_Count : Piece_Index) is limited
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
      Requests       : Piece_Interval_Vectors.Vector;
      Last_Request   : Natural;
      Last_Completed : Torrent.Piece_Count;
      Listener       : Connection_State_Listener_Access;
      Downloaded     : Piece_Offset;
      Piece_Map      : Boolean_Array (1 .. Piece_Count);
   end record;
end Torrent.Connections;
