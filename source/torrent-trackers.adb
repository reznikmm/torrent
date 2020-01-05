--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;

with League.Text_Codecs;

package body Torrent.Trackers is

   ---------------
   -- Event_URL --
   ---------------

   function Event_URL
     (Tracker     : League.IRIs.IRI;
      Info_Hash   : SHA1;
      Peer_Id     : SHA1;
      Port        : Positive;
      Uploaded    : Ada.Streams.Stream_Element_Count;
      Downloaded  : Ada.Streams.Stream_Element_Count;
      Left        : Ada.Streams.Stream_Element_Count;
      Event       : Announcement_Kind) return League.IRIs.IRI
   is
      subtype SHA1_URL_Encoded is Wide_Wide_String (1 .. 60);

      function URL_Encoded (Value : SHA1) return SHA1_URL_Encoded;

      -----------------
      -- URL_Encoded --
      -----------------

      function URL_Encoded (Value : SHA1) return SHA1_URL_Encoded is
         Text : constant SHA1_Image := Image (Value);
      begin
         return Result : SHA1_URL_Encoded do
            for J in 0 .. 19 loop
               Result (3 * J + 1) := '%';
               Result (3 * J + 2) := Text (2 * J + 1);
               Result (3 * J + 3) := Text (2 * J + 2);
            end loop;
         end return;
      end URL_Encoded;

      Port_Img : constant Wide_Wide_String := Positive'Wide_Wide_Image (Port);
      Up_Img   : constant Wide_Wide_String :=
        Ada.Streams.Stream_Element_Count'Wide_Wide_Image (Uploaded);
      Down_Img : constant Wide_Wide_String :=
        Ada.Streams.Stream_Element_Count'Wide_Wide_Image (Downloaded);
      Left_Img : constant Wide_Wide_String :=
        Ada.Streams.Stream_Element_Count'Wide_Wide_Image (Left);
      Query  : League.Strings.Universal_String := Tracker.Query;
      Result : League.IRIs.IRI := Tracker;
   begin
      Query.Append (if Query.Is_Empty then "?" else "&");
      Query.Append ("info_hash=");
      Query.Append (URL_Encoded (Info_Hash));
      Query.Append ("&peer_id=");
      Query.Append (URL_Encoded (Peer_Id));
      Query.Append ("&port=");
      Query.Append (Port_Img (2 .. Port_Img'Last));
      Query.Append ("&uploaded=");
      Query.Append (Up_Img (2 .. Up_Img'Last));
      Query.Append ("&downloaded=");
      Query.Append (Down_Img (2 .. Down_Img'Last));
      Query.Append ("&left=");
      Query.Append (Left_Img (2 .. Left_Img'Last));
      Query.Append ("&compact=1");

      case Event is
         when Started =>
            Query.Append ("&event=");
            Query.Append ("started");
         when Completed =>
            Query.Append ("&event=");
            Query.Append ("completed");
         when Stopped =>
            Query.Append ("&event=");
            Query.Append ("stopped");
         when Regular =>
            null;
      end case;

      Result.Set_Query (Query);

      return Result;
   end Event_URL;

   --------------------
   -- Failure_Reason --
   --------------------

   function Failure_Reason
     (Self : Response'Class) return League.Strings.Universal_String is
   begin
      return Self.Failure_Reason;
   end Failure_Reason;

   --------------
   -- Interval --
   --------------

   function Interval (Self : Response'Class) return Duration is
   begin
      return Self.Interval;
   end Interval;

   ----------------
   -- Is_Failure --
   ----------------

   function Is_Failure (Self : Response'Class) return Boolean is
   begin
      return Self.Is_Failure;
   end Is_Failure;

   -----------
   -- Parse --
   -----------

   function Parse (Data : Ada.Streams.Stream_Element_Array) return Response is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Count;
      use type League.Strings.Universal_String;

      subtype Digit is Ada.Streams.Stream_Element
        range Character'Pos ('0') .. Character'Pos ('9');

      function "+"
        (Text : Wide_Wide_String) return League.Strings.Universal_String
           renames League.Strings.To_Universal_String;

      Buffer : Ada.Streams.Stream_Element_Array renames Data;

      Next : Ada.Streams.Stream_Element_Count := Buffer'First;

      Error    : constant String := "Can't parse tracker's reply.";
      Codec    : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");

      package Peer_Vectors is new Ada.Containers.Vectors
        (Positive, Peer);

      procedure Expect (Char : Ada.Streams.Stream_Element);

      procedure Parse_Int (Value : out Integer);
      procedure Parse_String (Value : out League.Strings.Universal_String);
      procedure Parse_Peers_String (Result : out Peer_Vectors.Vector);
      procedure Parse_IP (Value : out League.Strings.Universal_String);
      procedure Parse_Port (Value : out Natural);

      package Constants is
         Interval : constant League.Strings.Universal_String := +"interval";
         Peers    : constant League.Strings.Universal_String := +"peers";
         Failure  : constant League.Strings.Universal_String :=
           +"failure reason";
      end Constants;

      ------------
      -- Expect --
      ------------

      procedure Expect (Char : Ada.Streams.Stream_Element) is
      begin
         if Buffer (Next) = Char then

            Next := Next + 1;
         else
            raise Constraint_Error with Error;
         end if;
      end Expect;

      ---------------
      -- Parse_Int --
      ---------------

      procedure Parse_Int (Value : out Integer) is
      begin
         Expect (Character'Pos ('i'));
         Value := 0;

         while Buffer (Next) in Digit loop
            Value := Value * 10
              + Integer (Buffer (Next))
              - Character'Pos ('0');

            Expect (Buffer (Next));
         end loop;

         Expect (Character'Pos ('e'));
      end Parse_Int;

      --------------
      -- Parse_IP --
      --------------

      procedure Parse_IP (Value : out League.Strings.Universal_String) is
         X1 : constant Wide_Wide_String :=
           Ada.Streams.Stream_Element'Wide_Wide_Image (Buffer (Next));
         X2 : Wide_Wide_String :=
           Ada.Streams.Stream_Element'Wide_Wide_Image (Buffer (Next + 1));
         X3 : Wide_Wide_String :=
           Ada.Streams.Stream_Element'Wide_Wide_Image (Buffer (Next + 2));
         X4 : Wide_Wide_String :=
           Ada.Streams.Stream_Element'Wide_Wide_Image (Buffer (Next + 3));
      begin
         X2 (1) := '.';
         X3 (1) := '.';
         X4 (1) := '.';
         Value.Clear;
         Value.Append (X1 (2 .. X1'Last));
         Value.Append (X2);
         Value.Append (X3);
         Value.Append (X4);

         Next := Next + 4;
      end Parse_IP;

      ------------------------
      -- Parse_Peers_String --
      ------------------------

      procedure Parse_Peers_String (Result : out Peer_Vectors.Vector) is
         Len : Ada.Streams.Stream_Element_Count := 0;
      begin
         while Buffer (Next) in Digit loop
            Len := Len * 10
              + Ada.Streams.Stream_Element_Count (Buffer (Next))
              - Character'Pos ('0');

            Expect (Buffer (Next));
         end loop;

         if Len mod 6 /= 0 then
            raise Constraint_Error with Error;
         end if;

         Expect (Character'Pos (':'));
         Result.Reserve_Capacity (Ada.Containers.Count_Type (Len / 6));

         for J in 1 .. Len / 6 loop
            declare
               Item : Peer;
            begin
               Item.Id := (others => 0);
               Parse_IP (Item.Address);
               Parse_Port (Item.Port);
               Result.Append (Item);
            end;
         end loop;
      end Parse_Peers_String;

      ----------------
      -- Parse_Port --
      ----------------

      procedure Parse_Port (Value : out Natural) is
      begin
         Value := Natural (Buffer (Next)) * 256 + Natural (Buffer (Next + 1));
         Next := Next + 2;
      end Parse_Port;

      ------------------
      -- Parse_String --
      ------------------

      procedure Parse_String (Value : out League.Strings.Universal_String) is
         Len : Ada.Streams.Stream_Element_Count := 0;
      begin
         while Buffer (Next) in Digit loop
            Len := Len * 10
              + Ada.Streams.Stream_Element_Count (Buffer (Next))
              - Character'Pos ('0');

            Expect (Buffer (Next));
         end loop;

         Expect (Character'Pos (':'));

         Value := Codec.Decode (Buffer (Next .. Next + Len - 1));
         Next := Next + Len;
      end Parse_String;

      Key      : League.Strings.Universal_String;
      Failure  : League.Strings.Universal_String;
      Interval : Integer := 0;
      Peers    : Peer_Vectors.Vector;
      Ignore   : Integer;
   begin
      Expect (Character'Pos ('d'));

      while Buffer (Next) /= Character'Pos ('e') loop
         Parse_String (Key);

         if Key = Constants.Failure then
            Parse_String (Failure);

         elsif Key = Constants.Interval then
            Parse_Int (Interval);

         elsif Key = Constants.Peers then
            if Buffer (Next) in Digit then
               Parse_Peers_String (Peers);
            else
               raise Constraint_Error with "Unimplemented peers reading";
            end if;
         elsif Buffer (Next) = Character'Pos ('i') then
            Parse_Int (Ignore);

         else

            raise Constraint_Error with Error;
         end if;
      end loop;

      Expect (Character'Pos ('e'));

      return Result : Response (Peer_Count => Peers.Last_Index) do
         Result.Is_Failure := not Failure.Is_Empty;
         Result.Failure_Reason := Failure;
         Result.Interval := Duration (Interval);

         for J in Result.Peers'Range loop
            Result.Peers (J) := Peers.Element (J);
         end loop;
      end return;
   end Parse;

   ------------------
   -- Peer_Address --
   ------------------

   function Peer_Address
     (Self  : Response'Class;
      Index : Positive) return League.Strings.Universal_String is
   begin
      return Self.Peers (Index).Address;
   end Peer_Address;

   ----------------
   -- Peer_Count --
   ----------------

   function Peer_Count (Self : Response'Class) return Natural is
   begin
      return Self.Peer_Count;
   end Peer_Count;

   -------------
   -- Peer_Id --
   -------------

   function Peer_Id (Self : Response'Class; Index : Positive) return SHA1 is
   begin
      return Self.Peers (Index).Id;
   end Peer_Id;

   ---------------
   -- Peer_Port --
   ---------------

   function Peer_Port
     (Self : Response'Class; Index : Positive) return Natural is
   begin
      return Self.Peers (Index).Port;
   end Peer_Port;

end Torrent.Trackers;
