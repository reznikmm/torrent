--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Interrupts.Names;

package Torrent.Shutdown is

   pragma Unreserve_All_Interrupts;

   protected Signal is
      entry Wait_SIGINT;

      procedure Handle;
      pragma Interrupt_Handler (Handle);
      pragma Attach_Handler (Handle, Ada.Interrupts.Names.SIGINT);
   private
      SIGINT_Triggered : Boolean := False;
   end Signal;

end Torrent.Shutdown;
