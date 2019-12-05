--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;

with League.IRIs;
with League.String_Vectors;
with League.Strings;

package Torrent.Metainfo_Files is

   type Metainfo_File (<>) is tagged private;
   --  Metainfo files also known as .torrent files

   not overriding function Read
     (File_Name : League.Strings.Universal_String)
      return Metainfo_File;
   --  Read and decode given metainfo file

   not overriding function Announce
     (Self : Metainfo_File) return League.IRIs.IRI;
   --  The URL of the tracker.

   not overriding function Name
     (Self : Metainfo_File) return League.Strings.Universal_String;
   --  The suggested name to save the file (or directory) as. It is purely
   --  advisory.

   not overriding function Piece_Length
     (Self : Metainfo_File) return Ada.Streams.Stream_Element_Count;
   --  The number of bytes in each piece the file is split into.

   not overriding function Piece_Count (Self : Metainfo_File) return Positive;
   --  Number of pieces

   not overriding function Piece_SHA1
     (Self  : Metainfo_File;
      Index : Positive) return SHA1
     with Pre => Index <= Self.Piece_Count;
   --  The SHA1 hash of the piece at the corresponding index.

   not overriding function File_Count (Self : Metainfo_File) return Positive;
   --  Number of files

   not overriding function File_Length
     (Self  : Metainfo_File;
      Index : Positive) return Ada.Streams.Stream_Element_Count
     with Pre => Index <= Self.File_Count;
   --  The length of the file in bytes.

   not overriding function File_Path
     (Self  : Metainfo_File;
      Index : Positive) return League.String_Vectors.Universal_String_Vector
     with Pre => Index <= Self.File_Count;
   --  A list of strings corresponding to subdirectory names, the last of
   --  which is the actual file name.

   not overriding function Info_Hash (Self  : Metainfo_File) return SHA1;
   --  The hash of the bencoded form of the info value from the metainfo file.

private

   type SHA1_Array is array (Positive range <>) of SHA1;

   type File_Information is record
      Length : Ada.Streams.Stream_Element_Count;
      Path   : League.String_Vectors.Universal_String_Vector;
   end record;

   type File_Information_Array is array (Positive range <>)
     of File_Information;

   type Metainfo_File
     (Piece_Count : Positive;
      File_Count  : Positive) is tagged
   record
      Announce     : League.IRIs.IRI;
      Name         : League.Strings.Universal_String;
      Piece_Length : Ada.Streams.Stream_Element_Count;
      Info_Hash    : SHA1;
      Hashes       : SHA1_Array (1 .. Piece_Count);
      Files        : File_Information_Array (1 .. File_Count);
   end record;

end Torrent.Metainfo_Files;
