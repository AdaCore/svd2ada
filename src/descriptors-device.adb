------------------------------------------------------------------------------
--                                                                          --
--                          SVD Binding Generator                           --
--                                                                          --
--                    Copyright (C) 2015-2016, AdaCore                      --
--                                                                          --
-- SVD2Ada is free software;  you can  redistribute it  and/or modify it    --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version.  SVD2Ada is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTA-  --
-- BILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public  --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General Public License  distributed with SVD2Ada; see file COPYING3.  If --
-- not, go to http://www.gnu.org/licenses for a complete copy of the        --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;            use Interfaces;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.OS_Lib;

with DOM.Core;              use DOM.Core;
with DOM.Core.Elements;     use DOM.Core.Elements;
with DOM.Core.Nodes;

with Ada_Gen;               use Ada_Gen;
with SVD2Ada_Utils;

package body Descriptors.Device is

   package Interrupt_Sort is new Interrupt_Vectors.Generic_Sorting
     (Base_Types."<");

   -----------------
   -- Read_Device --
   -----------------

   function Read_Device
     (Elt      : DOM.Core.Element;
      Pkg_Name : String) return Device_T
   is
      List : constant Node_List := Nodes.Child_Nodes (Elt);
      Ret  : Device_T;

   begin
      if Pkg_Name'Length > 0 then
         Ret.Name :=
           Ada.Strings.Unbounded.To_Unbounded_String (Pkg_Name);
         Base_Types.Base_Package := Ret.Name;
      end if;

      for J in 0 .. Nodes.Length (List) - 1 loop
         if Nodes.Node_Type (Nodes.Item (List, J)) = Element_Node then
            declare
               Child : constant Element := Element (Nodes.Item (List, J));
               Tag   : String renames Elements.Get_Tag_Name (Child);
            begin
               if Tag = "name" then
                  if Ada.Strings.Unbounded.Length (Ret.Name) = 0 then
                     Ret.Name := Get_Value (Child);
                  end if;

                  Base_Types.Base_Package := Ret.Name;

               elsif Tag = "version" then
                  Ret.Version := Get_Value (Child);

               elsif Tag = "description" then
                  Ret.Description := Get_Value (Child);

               elsif Tag = "addressUnitBits" then
                  Ret.Address_Unit_Bits := Get_Value (Child);

               elsif Tag = "width" then
                  Ret.Width := Get_Value (Child);

               elsif Register_Properties.Is_Register_Property (Tag) then
                  Register_Properties.Read_Register_Property
                    (Child, Ret.Reg_Properties);

               elsif Tag = "vendor" then
                  null; --  No need to decode, at least for now

               elsif Tag = "series" then
                  null; --  No need to decode, at least for now

               elsif Tag = "cpu" then
                  null; --  No need to decode, at least for now

               elsif Tag = "peripherals" then
                  declare
                     Child_List : constant Node_List :=
                                    Nodes.Child_Nodes (Child);
                     Peripheral : Peripheral_T;
                  begin
                     for K in 0 .. Nodes.Length (Child_List) - 1 loop
                        if Nodes.Node_Type (Nodes.Item (Child_List, K)) =
                          Element_Node
                        then
                           Peripheral :=
                             Read_Peripheral
                               (Element (Nodes.Item (Child_List, K)),
                                Ret.Reg_Properties,
                                Ret.Peripherals);
                           Ret.Peripherals.Append (Peripheral);
                        end if;
                     end loop;
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("*** WARNING: ignoring device element " & Tag);
               end if;
            end;
         end if;
      end loop;

      return Ret;
   end Read_Device;

   ----------------------
   -- Dump_Handler_ASM --
   ----------------------

   procedure Dump_Handler_ASM
     (Device     : Device_T;
      Ints       : Interrupt_Vectors.Vector;
      Output_Dir : String)
   is
      ASM      : File_Type;
      F_Name   : constant String :=
                   GNAT.OS_Lib.Normalize_Pathname ("handler.S", Output_Dir);
      Tab_Size : Natural;
      Curs     : Interrupt_Vectors.Cursor;

      procedure Dump_Stub (Name : String);
      --  Dumps a weak empty function. To be overwritten by the run-time

      ---------------
      -- Dump_Stub --
      ---------------

      procedure Dump_Stub (Name : String) is
      begin
         Put_Line (ASM, ASCII.HT & ".thumb_func");
         Put_Line (ASM, ".weak " & Name);
         Put_Line (ASM, ".type " & Name & ", %function");
         Put_Line (ASM, Name & ":");
         Put_Line (ASM, "0:" & ASCII.HT & "b 0b");
         Put_Line (ASM, ASCII.HT & ".size " & Name & ", . - " & Name);
         New_Line (ASM);
      end Dump_Stub;

   begin
      Create (ASM, Out_File, F_Name);
      Put_Line (ASM, "## -*- asm -*- #############################");
      Put_Line (ASM, "# Automatically generated by SVD2Ada");
      Put_Line (ASM, "# For the " &
                  Ada.Strings.Unbounded.To_String (Device.Description) &
                  " target");
      Put_Line (ASM, "############################################");
      New_Line (ASM);
      Put_Line (ASM, ASCII.HT & ".syntax unified");
      --  ??? target is m4/m7, but what about previous versions?
      Put_Line (ASM, ASCII.HT & ".cpu cortex-m4");
      Put_Line (ASM, ASCII.HT & ".thumb");
      New_Line (ASM);

      --  Calculate the vector size to properly align it as requested.
      --  The vector consists on 16 reserved values for RESET, Hard float and
      --  so on, + the IRQs
      --  Removing 2 from the GNAT IRQ IDs to obtain the actual number of
      --  IRQs (the GNAT ID includes Systick, 0 being reserved).
      Tab_Size := (16 + Natural (Ints.Last_Element.Value) - 2) * 4;
      --  Now essie the proper alignment directive: the table needs to be
      --  aligned on a power of 2.
      Put_Line (ASM, ASCII.HT & ".text");
      Put_Line (ASM, ASCII.HT & ".globl __vectors");

      for J in 5 .. 16 loop
         if 2 ** J >= Tab_Size then
            Put_Line (ASM, ASCII.HT & ".p2align" & J'Img);
            exit;
         end if;
      end loop;

      Put_Line (ASM, "__vectors:");
      --  Cortex ARMv7-m defined 16 first values
      Put_Line
        (ASM,
         ASCII.HT & "/* Cortex-M core interrupts */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   0                    /* stack top address */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 1 Reset.  */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 2 NMI. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 3 Hard fault. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 4 Mem manage. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 5 Bus fault. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 6 Usage fault. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 7 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 8 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 9 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 10 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   __gnat_sv_call_trap  /* 11 SVCall. */");
      Put_Line
        (ASM,
         ASCII.HT &
           ".word   fault                /* 12 reserved for debug. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   fault                /* 13 reserved. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   __gnat_pend_sv_trap  /* 14 PendSV. */");
      Put_Line
        (ASM,
         ASCII.HT & ".word   __gnat_sys_tick_trap /* 15 Systick. */");

      Put_Line
        (ASM,
         ASCII.HT & "/* MCU interrupts */");

      Curs := Ints.First;

      for J in 2 .. Ints.Last_Element.Value loop
         while Interrupt_Vectors.Element (Curs).Value < J loop
            Interrupt_Vectors.Next (Curs);
         end loop;

         Put
           (ASM, ASCII.HT & ".word __gnat_irq_trap        /*" &
              Unsigned'Image (J - 2 + 16) & " ");
         if Interrupt_Vectors.Element (Curs).Value = J then
            Put_Line
              (ASM,
               Ada.Strings.Unbounded.To_String
                 (Interrupt_Vectors.Element (Curs).Name) &
                 " */");
         else
            Put_Line
              (ASM, "IRQ" & Unsigned'Image (J - 2) & ". */");
         end if;
      end loop;

      New_Line (ASM);
      Put_Line (ASM, ASCII.HT & ".text");
      New_Line (ASM);
      --  Add weak symbols for the functions denoted by the Vector
      Dump_Stub ("__gnat_irq_trap");
      Dump_Stub ("__gnat_sv_call_trap");
      Dump_Stub ("__gnat_pend_sv_trap");
      Dump_Stub ("__gnat_sys_tick_trap");

      --  Add the never-ending 'fault' function
      Put_Line (ASM, ASCII.HT & ".thumb_func");
      Put_Line (ASM, "fault:" & ASCII.HT & "b fault");
      Close (ASM);
   end Dump_Handler_ASM;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Device     : Device_T;
      Output_Dir : String)
   is
      use Ada.Strings.Unbounded;
      Peripherals : Peripheral_Vectors.Vector;
      Interrupts  : Interrupt_Vectors.Vector;
      Spec        : Ada_Gen.Ada_Spec :=
                      New_Spec (To_String (Device.Name),
                                To_String (Device.Description),
                                True);
      Old_Spec    : Ada_Gen.Ada_Spec;
      Max_Len     : Natural := 0;
      Gen_RT_IRQ  : constant Boolean := Is_Interfaces_Hierarchy (Spec);
      --  Whether we generate Run-Time support files for IRQ handling
      --  This is activated when generating in the Interfaces hierarchy

   begin
      ----------------
      -- Interrupts --
      ----------------

      if Gen_RT_IRQ then
         Old_Spec := Spec;
         --  When generating stubs for the Interfaces run-time hierarchy, also
         --  generate the Ada.Exceptions.Name file from the interrupts list
         Spec := New_Spec ("Ada.Interrupts.Names",
                           "This is a version for the " &
                             To_String (Device.Description) & " MCU",
                           False);
         Add (Spec,
              New_Pragma
                ("Implementation_Defined",
                 "All identifiers in this unit are implementation defined"));

         --  Add core interrupts
         Interrupts.Append
           ((Name        => To_Unbounded_String ("Sys_Tick"),
             Description => To_Unbounded_String
               ("The position of the interrupts are documented as starting " &
                  "at 0. Unfortunately, Interrupt_Id 0 is reserved and the " &
                  "SysTick interrupt (a core interrupt) is handled by the " &
                  "runtime like other interrupts. So IRQ 0 is numbered 2 " &
                  "while it is at position 0 in the manual. The offset of 2 " &
                  "is reflected in s-bbbosu.adb by the First_IRQ constant."),
             Value       => 1));

         if Slice (Device.Description, 1, 5) = "STM32" then
            --  ??? Workaround for the STM32F* svd files that do not define the
            --  FPU Interrupt
            Interrupts.Append
              ((Name        => To_Unbounded_String ("FPU"),
                Description => To_Unbounded_String
                  ("FPU global interrupt"),
                Value       => 83));
         end if;

      else
         Old_Spec := Spec;
         Spec := New_Child_Spec ("Interrupts",
                                 To_String (Device.Name),
                                 "Definition of the device's interrupts",
                                 False);
         Add (Spec, New_With_Clause ("Ada.Interrupts", True));
      end if;

      Add (Spec, New_Comment_Box ("Interrupts"));

      for Periph of Device.Peripherals loop
         for Int of Periph.Interrupts loop
            if not Interrupts.Contains (Int) then
               Interrupts.Append (Int);
               Max_Len := Natural'Max (Length (Int.Name), Max_Len);
            end if;
         end loop;
      end loop;

      Interrupt_Sort.Sort (Interrupts);

      for Int of Interrupts loop
         if Length (Int.Name) > 4
           and then Slice (Int.Name, Length (Int.Name) - 3,
                           Length (Int.Name)) = "_IRQ"
         then
            Add (Spec,
                 New_Constant_Value
                   (Id       => Slice (Int.Name, 1, Length (Int.Name) - 4) &
                                  "_Interrupt",
                    Align_Id => Max_Len + 11,
                    Typ      => "Interrupt_ID",
                    Value    => To_String (Integer (Int.Value)),
                    Comment  => To_String (Int.Description)));
         else
            Add (Spec,
                 New_Constant_Value
                   (Id       => To_String (Int.Name) & "_Interrupt",
                    Align_Id => Max_Len + 11,
                    Typ      => "Interrupt_ID",
                    Value    => To_String (Integer (Int.Value)),
                    Comment  => To_String (Int.Description)));
         end if;
      end loop;

      if Gen_RT_IRQ then
         Ada_Gen.Write_Spec (Spec, Output_Dir);
         Dump_Handler_ASM (Device, Interrupts, Output_Dir);
      end if;

      Spec := Old_Spec;

      ----------------------------
      --  Base types definition --
      ----------------------------

      if SVD2Ada_Utils.External_Base_Types_Package then
         --  From GNAT GPL 2016 and GNAT Pro 17, Interfaces.Bit_Types is
         --  defined
         Ada_Gen.Add_Global_With (SVD2Ada_Utils.Base_Types_Package);
      else
         Add (Spec, New_Comment_Box ("Base type"));
         Add_No_Check
           (Spec, New_Type_Scalar (Target_Type (32, False), 32));
         Add_No_Check
           (Spec, New_Type_Scalar (Target_Type (16, False), 16));
         Add_No_Check
           (Spec, New_Type_Scalar (Target_Type (8, False), 8));
         Add_No_Check
           (Spec, New_Type_Scalar (Target_Type (1, False), 1));

         for J in 2 .. Device.Width loop
            if J /= 8 and then J /= 16 and then J /= 32 then
               Add_No_Check
                 (Spec, New_Type_Scalar (Target_Type (J, False), J));
            end if;
         end loop;
      end if;

      -----------------------------------------
      --  Base addresses for the peripherals --
      -----------------------------------------

      Add (Spec, New_Comment_Box ("Base addresses"));
      Add (Spec, New_With_Clause ("System", False));

      for Periph of Device.Peripherals loop
         Add (Spec,
              New_Constant_Value
                (Id       => To_String (Periph.Name) & "_Base",
                 Align_Id => 0,
                 Typ      => "System.Address",
                 Value    => "System'To_Address (" &
                   To_Hex (Periph.Base_Address) & ")"));
      end loop;

      Ada_Gen.Write_Spec (Spec, Output_Dir);

      Write_Spec (Spec, Output_Dir);

      Peripherals := Device.Peripherals;

      while not Peripherals.Is_Empty loop
         declare
            P     : Peripheral_T := Peripherals.First_Element;
            Vec   : Peripheral_Vectors.Vector;
            Index : Natural;
         begin
            Peripherals.Delete_First;

            if Ada.Strings.Unbounded.Length (P.Group_Name) = 0 then
               Dump (P,
                     Ada.Strings.Unbounded.To_String (Device.Name),
                     Output_Dir);
            else
               Vec.Append (P);
               Index := Peripherals.First_Index;

               while Index <= Peripherals.Last_Index loop
                  if Peripherals (Index).Group_Name = P.Group_Name then
                     Vec.Append (Peripherals (Index));
                     Peripherals.Delete (Index);
                  else
                     Index := Index + 1;
                  end if;
               end loop;

               Dump (Vec, Ada.Strings.Unbounded.To_String (Device.Name),
                     Output_Dir);
            end if;
         end;
      end loop;
   end Dump;

end Descriptors.Device;
