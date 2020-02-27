--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

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
      Id   : Torrent.SHA1;
      Path : League.Strings.Universal_String) is
   begin
      Self.Path := Path;
      Self.Peer_Id := Id;
   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self        : in out Context'Class;
      Next_Update : out Ada.Calendar.Time)
   is
      use type Ada.Calendar.Time;

   begin
      Next_Update := Ada.Calendar.Clock + 3600.0;

      for J of Self.Downloaders loop
         J.Start;
      end loop;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Context'Class) is
   begin
      for J of Self.Downloaders loop
         J.Stop;
      end loop;

      Self.Manager.Complete;
      Self.Initiator.Stop;
   end Stop;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self        : in out Context'Class;
      Next_Update : out Ada.Calendar.Time)
   is
      use type Ada.Calendar.Time;

   begin
      Next_Update := Ada.Calendar.Clock + 3600.0;

      for J of Self.Downloaders loop
         J.Update;
      end loop;
   end Update;

end Torrent.Contexts;
