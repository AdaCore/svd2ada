------------------------------------------------------------------------------
--                              SVD Binding Generator                       --
--                                                                          --
--            Copyright (C) 2015, Simon Wright <simon@pushface.org>         --
--                                                                          --
--  This tool is free software;  you can redistribute it and/or modify      --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
------------------------------------------------------------------------------

with Base_Types;

package SVD2Ada_Options is

   --  Command line options

   --  AdaCore's RTS adds 2 to the actual IRQ number because it treats
   --  Systick using the same mechanisms as the MCU interrrupts.
   Use_IRQ_Offset : Base_Types.Unsigned := 2;

   -- If False, use the non-standard Volatile_Full_Access aspect.
   Use_Standard_Volatile_Aspect : aliased Boolean := False;

end SVD2Ada_Options;
