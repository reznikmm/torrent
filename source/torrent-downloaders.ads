--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;

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

   procedure Start
     (Self : aliased in out Downloader'Class;
      Port : Positive;
      Path : League.String_Vectors.Universal_String_Vector);

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

   protected type Tracked_Pieces (Piece_Count : Piece_Index) is
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
         Tracked          : aliased Tracked_Pieces (Piece_Count);
         Peer_Id          : SHA1;
         Port             : Positive;
         Uploaded         : Ada.Streams.Stream_Element_Count;
         Downloaded       : Ada.Streams.Stream_Element_Count;
         Left             : Ada.Streams.Stream_Element_Count;
--         Last_Piece_Size  : Positive;
         Chocked          : Connection_Vectors.Vector;

         Storage : aliased Torrent.Storages.Storage (Meta, File_Count);
      end record;

end Torrent.Downloaders;
