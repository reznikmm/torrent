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

      procedure Best_Connections
        (Chocked : Connection_Vectors.Vector;
         Result  : out Connection_Access_Array);

      ----------------------
      -- Best_Connections --
      ----------------------

      procedure Best_Connections
        (Chocked : Connection_Vectors.Vector;
         Result  : out Connection_Access_Array)
      is
         Index : Positive := Result'First;
      begin
         Result := (Result'Range => null);

         for X of Chocked loop
            if X.Intrested then
               Result (Index) := X;

               exit when Index = Result'Last;

               Index := Index + 1;
            end if;
         end loop;
      end Best_Connections;

      Seeders  : array (1 .. 1) of Seeder;
      Slowdown : Duration := 0.5;
      Chocked  : Connection_Vectors.Vector;
   begin
      loop
         select
            accept Connected
              (Value : in not null Torrent.Connections.Connection_Access)
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
            Best_Connections (Chocked, List);

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
