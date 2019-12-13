--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

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
      Piece_Count : Positive) is
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

   package Piece_State_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Torrent.Connections.Piece_State,
      "="          => Torrent.Connections."=");

   protected type Tracked_Pieces (Piece_Count : Positive) is
      new Torrent.Connections.Connection_State_Listener with

      procedure Initialize (Piece_Size_Value : Piece_Offset);

      overriding procedure Reserve_Intervals
        (Connection : Torrent.Connections.Connection_Access;
         Map        : Boolean_Array;
         Value      : out Torrent.Connections.Piece_State);

      overriding function We_Are_Intrested
        (Map : Boolean_Array) return Boolean;

   private
      Our_Map         : Boolean_Array (1 .. Piece_Count) := (others => False);
      Piece_Size      : Piece_Offset;
      Last_Piece_Size : Piece_Offset;

      Unfinished : Piece_State_Vectors.Vector;
   end Tracked_Pieces;

   type Downloader
     (Meta       : not null Torrent.Metainfo_Files.Metainfo_File_Access;
      File_Count : Ada.Containers.Count_Type;
      Piece_Count : Positive) is
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
         Last_Piece_Size  : Positive;
         Chocked          : Connection_Vectors.Vector;

         Storage : aliased Torrent.Storages.Storage (Meta, File_Count);
      end record;

end Torrent.Downloaders;
