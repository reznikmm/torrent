--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar.Formatting;

with GNAT.SHA1;

package body Torrent.Contexts is

   -----------------------
   -- Add_Metainfo_File --
   -----------------------

   procedure Add_Metainfo_File
     (Self : in out Context'Class;
      File : not null Torrent.Metainfo_Files.Metainfo_File_Access)
   is
      File_Count : constant Ada.Containers.Count_Type :=
        Ada.Containers.Count_Type (File.File_Count);

      Job : constant Downloader_Access := new Torrent.Downloaders.Downloader
        (Context     => Self'Unchecked_Access,
         Meta        => File,
         File_Count  => File_Count,
         Piece_Count => File.Piece_Count);
   begin
      Job.Initialize (Self.Peer_Id, Self.Path);
      Self.Downloaders.Insert (File.Info_Hash, Job);
   end Add_Metainfo_File;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Self    : Context'Class;
      Job     : Torrent.Downloaders.Downloader_Access;
      Address : GNAT.Sockets.Sock_Addr_Type) is
   begin
      Self.Initiator.Connect (Job, Address);
   end Connect;

   ---------------
   -- Connected --
   ---------------

   procedure Connected
     (Self  : Context'Class;
      Value : Torrent.Connections.Connection_Access) is
   begin
      Self.Manager.Connected (Value);
   end Connected;

   -------------------
   -- Find_Download --
   -------------------

   function Find_Download
     (Self : Context'Class;
      Hash : SHA1) return Torrent.Downloaders.Downloader_Access
   is
      Cursor : constant Downloader_Maps.Cursor := Self.Downloaders.Find (Hash);
   begin
      if Downloader_Maps.Has_Element (Cursor) then
         return Torrent.Downloaders.Downloader_Access
           (Downloader_Maps.Element (Cursor));
      else
         return null;
      end if;
   end Find_Download;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Context'Class;
      Path : League.Strings.Universal_String)
   is
      procedure Set_Peer_Id (Value : out SHA1);

      -----------------
      -- Set_Peer_Id --
      -----------------

      procedure Set_Peer_Id (Value : out SHA1) is
         Now : constant String := Ada.Calendar.Formatting.Image
           (Ada.Calendar.Clock);
         Context : GNAT.SHA1.Context;
      begin
         GNAT.SHA1.Update (Context, Path.To_UTF_8_String);
         GNAT.SHA1.Update (Context, Now);
         GNAT.SHA1.Update (Context, GNAT.Sockets.Host_Name);

         Value := GNAT.SHA1.Digest (Context);
      end Set_Peer_Id;

   begin
      Self.Path := Path;
      Set_Peer_Id (Self.Peer_Id);
   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Context'Class) is
   begin
      for J of Self.Downloaders loop
         J.Start;
      end loop;

      delay 3600.0;  --  Seed file for some time

      Self.Manager.Complete;
      Self.Initiator.Stop;
   end Start;

end Torrent.Contexts;
