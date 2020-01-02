--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;

with Torrent.Downloaders; use Torrent.Downloaders;

package body Torrent.Managers is

   package Connection_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Torrent.Connections.Connection_Access,
      "="          => Torrent.Connections."=");

   task type Seeder is
      entry Seed (Value : not null Torrent.Connections.Connection_Access);
      entry Stop_Seeding;
      entry Stop;
   end Seeder;

   type Connection_Access_Array is
     array (Positive range <>) of Torrent.Connections.Connection_Access;

   -------------
   -- Manager --
   -------------

   task body Manager is
      use type Torrent.Connections.Connection_Access;

      Default_Slowdown : constant := 0.5;

      type Protection_Information is record
         Connection : Torrent.Connections.Connection_Access;
         --  This connection is protected for next Round rounds
         Round      : Natural := 0;  --  Zero means unprotected anymore
         Found      : Boolean := False;  --  We have found the Connection
         Skip_Count : Positive := 1;
         --  How much connections we should skip to find next candidate
         Skipped    : Natural := 0;  --  How much we have skipped
         Candidate  : Torrent.Connections.Connection_Access;
         --  A candidate to protected position
         Fallback   : Torrent.Connections.Connection_Access;
         --  A second candidate to protected position
      end record;
      --  Protected connection is unchoked regardless of its upload rate for
      --  30 seconds (3 rounds x 10 seconds).

      procedure Best_Connections
        (Chocked    : Connection_Vectors.Vector;
         Protection : in out Protection_Information;
         Result     : out Connection_Access_Array);
      --  Select a few connection to seed them some data.

      ----------------------
      -- Best_Connections --
      ----------------------

      procedure Best_Connections
        (Chocked    : Connection_Vectors.Vector;
         Protection : in out Protection_Information;
         Result     : out Connection_Access_Array)
      is
         procedure New_Round (Protection : in out Protection_Information);
         --  Adjust Protection_Information for a new cycle

         procedure Update_Protection
           (Protection : in out Protection_Information;
            Connection : Torrent.Connections.Connection_Access);
         --  Update protection information

         procedure Append
           (Connection : Torrent.Connections.Connection_Access;
            Protection : in out Protection_Information;
            C_Rate     : Piece_Offset);
         --  Append a connection to Result taking its Rate into account.
         --  Update protection information if some connection is skipped.

         ---------------
         -- New_Round --
         ---------------

         procedure New_Round (Protection : in out Protection_Information) is
         begin
            Protection.Found := False;
            Protection.Skipped := 0;
            Protection.Candidate := null;
            Protection.Fallback := null;
         end New_Round;

         -----------------------
         -- Update_Protection --
         -----------------------

         procedure Update_Protection
           (Protection : in out Protection_Information;
            Connection : Torrent.Connections.Connection_Access) is
         begin
            if Protection.Fallback = null then
               --  Remember very first skipped connection as fallback
               Protection.Fallback := Connection;
            elsif Protection.Skip_Count = Protection.Skipped then
               --  Take candidate after skipping Skip_Count connections
               Protection.Candidate := Connection;
            end if;

            if Protection.Connection = Connection and Protection.Round > 0 then
               Protection.Found := True;
            end if;

            Protection.Skipped := Protection.Skipped + 1;
         end Update_Protection;

         Rate : array (Result'Range) of Piece_Offset;
         Last : Positive := Result'First - 1;

         ------------
         -- Append --
         ------------

         procedure Append
           (Connection : Torrent.Connections.Connection_Access;
            Protection : in out Protection_Information;
            C_Rate     : Piece_Offset)
         is
            use type Piece_Offset;
         begin
            for J in Result'First .. Last loop
               if Rate (J) < C_Rate then
                  --  Found rate worse then C, insert C into Result

                  if Last = Result'Last then
                     --  Result is full, skip last element
                     Update_Protection (Protection, Result (Last));
                     Result (J + 1 .. Last) := Result (J .. Last - 1);
                     Rate  (J + 1 .. Last) := Rate  (J .. Last - 1);
                  else
                     Result (J + 1 .. Last + 1) := Result (J .. Last);
                     Rate  (J + 1 .. Last + 1) := Rate  (J .. Last);
                     Last := Last + 1;
                  end if;

                  Result (J) := Connection;
                  Rate (J) := C_Rate;

                  return;
               end if;
            end loop;

            if Last < Result'Last then
               --  Append C to Result if there is free space
               Last := Last + 1;
               Result (Last) := Connection;
               Rate (Last) := C_Rate;
            end if;
         end Append;

      begin
         New_Round (Protection);
         Result := (Result'Range => null);

         for X of Chocked loop
            if X.Intrested then
               Append (X, Protection, X.Downloaded);
            end if;
         end loop;

         if Protection.Found then
            Append (Protection.Connection, Protection, Piece_Offset'Last);
            Protection.Round := Protection.Round - 1;
         elsif Protection.Candidate /= null then
            Append (Protection.Candidate, Protection, Piece_Offset'Last);
            Protection.Connection := Protection.Candidate;
            Protection.Round := 2;
            Protection.Skip_Count := Protection.Skip_Count + 1;
         elsif Protection.Fallback /= null then
            Append (Protection.Fallback, Protection, Piece_Offset'Last);
            Protection.Connection := Protection.Fallback;
            Protection.Round := 2;
            Protection.Skip_Count := 1;
         end if;
      end Best_Connections;

      Protection : Protection_Information;
      Seeders    : array (1 .. 4) of Seeder;
      Slowdown   : Duration := Default_Slowdown;
      Chocked    : Connection_Vectors.Vector;
   begin
      loop
         select
            accept Connected
              (Value : not null Torrent.Connections.Connection_Access)
            do
               Chocked.Append (Value);
               Slowdown := 0.0;
            end Connected;
         or
            accept Complete;
            exit;
         else
            delay Slowdown;
         end select;

         declare
            List : Connection_Access_Array (Seeders'Range);
         begin
            Best_Connections (Chocked, Protection, List);

            for J in List'Range loop
               if List (J) /= null then
                  Seeders (J).Seed (List (J));
               end if;
            end loop;

            --  process chocked connections.
            declare
               J : Positive := 1;
               Conn : Torrent.Connections.Connection_Access;
            begin
               while J <= Chocked.Last_Index loop
                  Conn := Chocked (J);

                  if not Conn.Connected then
                     Recycle.Enqueue (Conn);
                     Chocked.Delete (J);

                     if Chocked.Is_Empty then
                        Slowdown := Default_Slowdown;
                     end if;
                  elsif not (for some X of List => X = Conn) then
                     Conn.Serve (Conn.Downloader.Completed, 1.0);
                     J := J + 1;
                  else
                     J := J + 1;
                  end if;
               end loop;
            end;

            for J in List'Range loop
               if List (J) /= null then
                  Seeders (J).Stop_Seeding;
               end if;
            end loop;
         end;
      end loop;

      for J in Seeders'Range loop
         Seeders (J).Stop;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Manager;

   -------------
   -- Session --
   -------------

   task body Seeder is
      Seed_Time : constant Duration := 10.0;

      Conn : Torrent.Connections.Connection_Access;
   begin
      loop
         select
            accept Stop;
            exit;

         or
            accept Seed
              (Value : not null Torrent.Connections.Connection_Access)
            do
               Conn := Value;
            end Seed;

         end select;

         if Conn.Intrested then
            Conn.Set_Choked (False);
            Conn.Serve (Conn.Downloader.Completed, Seed_Time);
            Conn.Set_Choked (True);
         end if;

         accept Stop_Seeding;
      end loop;
   end Seeder;

end Torrent.Managers;
