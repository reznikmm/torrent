--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.String_Vectors;
with League.Strings;
with League.Stream_Element_Vectors;

with AWS.Client;
with AWS.Messages;
with AWS.Response;

with Torrent.Downloaders;
with Torrent.Metainfo_Files;

procedure Torrent.Run is
   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
         renames League.Strings.To_Universal_String;

   Cmd : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Meta : aliased Torrent.Metainfo_Files.Metainfo_File;
   DL   : Torrent.Downloaders.Downloader (Meta'Access);
   Path : League.String_Vectors.Universal_String_Vector;
begin
   Path.Append (+"tmp");
   Meta.Read (Cmd (1));

   Ada.Wide_Wide_Text_IO.Put_Line
     (Meta.Announce.To_Universal_String.To_Wide_Wide_String);

   DL.Initialize
     (Port => 12345,
      Path => Path);

   declare
      Request : Torrent.Downloaders.Request;
   begin
      DL.Get_Request (Request);

      case Request.Kind is
         when Torrent.Downloaders.No_Request =>
            null;
         when Torrent.Downloaders.Tracker_Request =>
            declare
               Reply : constant AWS.Response.Data :=
                 AWS.Client.Get
                   (Request.URL.To_Universal_String.To_UTF_8_String,
                    Follow_Redirection => True);
            begin
               if AWS.Response.Status_Code (Reply)
                    not in AWS.Messages.Success
               then
                  raise Program_Error;
               end if;

               DL.New_Response
                 ((Torrent.Downloaders.Tracker_Request,
                  League.Stream_Element_Vectors.To_Stream_Element_Vector
                    (AWS.Response.Message_Body (Reply)),
                     True));
            end;
         when Torrent.Downloaders.Peer_Request =>
            raise Program_Error;
      end case;
   end;
end Torrent.Run;
