--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Finalization;

with GNAT.Sockets;

with League.String_Vectors;

with Torrent.Connections;
with Torrent.Metainfo_Files;
with Torrent.Storages;
with Torrent.Trackers;

package Torrent.Downloaders is

   type Downloader
     (Meta        : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      File_Count  : Ada.Containers.Count_Type;
      Piece_Count : Piece_Index) is
        tagged limited private;
   --  The downloader tracks one torrent file and all connections
   --  related to it.

   type Downloader_Access is access all Downloader'Class
     with Storage_Size => 0;

   procedure Start
     (Self : aliased in out Downloader'Class;
      Path : League.String_Vectors.Universal_String_Vector);

   function Completed (Self : Downloader'Class)
     return Torrent.Connections.Piece_Index_Array
       with Inline;

   function Create_Session
     (Self    : in out Downloader'Class;
      Address : GNAT.Sockets.Sock_Addr_Type)
      return Torrent.Connections.Connection_Access;

   --  For interlnal usage

   package Connection_Queue_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces
       (Torrent.Connections.Connection_Access);

   package Connection_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Connection_Queue_Interfaces);

   Recycle : Connection_Queues.Queue;
   --  Connections, returned by Manager back to Initiator to recconnect or
   --  destroy.

   procedure Connected (Self : Torrent.Connections.Connection_Access);

   function Find_Download (Hash : SHA1) return Downloader_Access;

   Port : constant := 33411;

private

   package Connection_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Torrent.Connections.Connection_Access,
      "="          => Torrent.Connections."=");

   type Tracker_Response_Access is access Torrent.Trackers.Response;

   package Piece_State_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Piece_Index,
      Element_Type => Torrent.Connections.Interval_Vectors.Vector,
      "<"          => "<",
      "="          => Torrent.Connections.Interval_Vectors."=");

   protected type Tracked_Pieces
     (Downloader  : not null access Downloaders.Downloader;
      Piece_Count : Piece_Index) is
      new Torrent.Connections.Connection_State_Listener with

      procedure Initialize
        (Piece_Length      : Piece_Offset;
         Last_Piece_Length : Piece_Offset);

      overriding procedure Reserve_Intervals
        (Map        : Boolean_Array;
         Value      : out Torrent.Connections.Piece_State);

      overriding function We_Are_Intrested
        (Map : Boolean_Array) return Boolean;

      overriding procedure Interval_Saved
        (Piece : Piece_Index;
         Value : Torrent.Connections.Interval;
         Last  : out Boolean);

      overriding procedure Piece_Completed
        (Piece : Piece_Index;
         Ok    : Boolean);

      overriding procedure Unreserve_Intervals
        (Value : Torrent.Connections.Piece_Interval_Array);

   private
      Our_Map         : Boolean_Array (1 .. Piece_Count) := (others => False);
      Piece_Size      : Piece_Offset;
      Last_Piece_Size : Piece_Offset;

      Finished   : Piece_State_Maps.Map;
      Unfinished : Piece_State_Maps.Map;
   end Tracked_Pieces;

   type Downloader
     (Meta        : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      File_Count  : Ada.Containers.Count_Type;
      Piece_Count : Piece_Index) is
     new Ada.Finalization.Limited_Controlled with
      record
         Path             : League.String_Vectors.Universal_String_Vector;
         Tracker_Response : Tracker_Response_Access;
         Tracked          : aliased Tracked_Pieces
           (Downloader'Unchecked_Access, Piece_Count);
         Peer_Id          : SHA1;
         Port             : Positive;
         Uploaded         : Ada.Streams.Stream_Element_Count;
         Downloaded       : Ada.Streams.Stream_Element_Count;
         Left             : Ada.Streams.Stream_Element_Count;
         Chocked          : Connection_Vectors.Vector;
         Completed        : Torrent.Connections.Piece_Index_Array
           (1 .. Piece_Count);
         Last_Completed   : Torrent.Piece_Count;
         Storage : aliased Torrent.Storages.Storage (Meta, File_Count);
      end record;

end Torrent.Downloaders;
