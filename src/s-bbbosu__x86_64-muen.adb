------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2021, AdaCore                     --
--                     Copyright (C) 2023, codelabs GmbH                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

with System.BB.CPU_Specific;
with System.BB.Parameters;
with System.Machine_Code;

with Interfaces; use Interfaces;

with Musinfo.Instance;
with Musinfo.Utils;
with Muschedinfo;
with Mutimedevents;

package body System.BB.Board_Support is

   use System.BB.CPU_Specific;
   use System.BB.Parameters;
   use System.Machine_Code;

   Subject_Info_Virtual_Addr : constant := 16#000e_0000_0000#;
   Subject_Info_Size         : constant := 16#8000#;

   Sched_Info : Muschedinfo.Scheduling_Info_Type
   with
      Import,
      Volatile,
      Async_Writers,
      Address => System'To_Address
        (Subject_Info_Virtual_Addr + Subject_Info_Size);

   Timed_Evt_Addr : constant := 16#000e_0001_0000#;

   Timed_Evt : Mutimedevents.Timed_Event_Interface_Type
   with
      Import,
      Volatile,
      Address => System'To_Address (Timed_Evt_Addr);

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      use type Musinfo.Resource_Type;

      Event : Musinfo.Resource_Type;
   begin
      Event := Musinfo.Instance.Resource_By_Name
        (Name => Musinfo.Utils.To_Name (Str => "timer"),
         Kind => Musinfo.Res_Event);
      if Event = Musinfo.Null_Resource then
         raise Program_Error with "No timer event present";
      end if;

      Timed_Evt.Event_Nr := Mutimedevents.Unsigned_6'Mod
        (Event.Evt_Data.Value);
   end Initialize_Board;

   package body Interrupts is

      function Priority_From_Interrupt_ID
        (Interrupt : BB.Interrupts.Interrupt_ID)
        return Any_Priority;
      --  On x86-64 the priority of an Interrupt is encoded in the upper 4-bits
      --  of the 8-bit Interrupt ID.

      --------------------------------
      -- Priority_From_Interrupt_ID --
      --------------------------------

      function Priority_From_Interrupt_ID
        (Interrupt : BB.Interrupts.Interrupt_ID)
        return Any_Priority
      is
         function Shift_Right
           (Value  : Any_Priority;
            Amount : Natural) return Any_Priority
           with Import, Convention => Intrinsic;
         --  Used to retrieve the interrupt priority that is encoded in the
         --  top 4-bits of the Interrupt ID.
      begin
         return Shift_Right (System.Any_Priority (Interrupt), 4);
      end Priority_From_Interrupt_ID;

      -------------------------------
      -- Install_Interrupt_Handler --
      -------------------------------

      procedure Install_Interrupt_Handler
        (Interrupt : BB.Interrupts.Interrupt_ID;
         Prio      : Interrupt_Priority)
      is
      begin
         --  On x86-64 there's is nothing for us to do to enable the interrupt
         --  in the local APIC. However, we use this opportunity to check the
         --  priority of the protected object corresponds with the priority of
         --  the Interrupt Vector since the upper 4-bits of the 8-bit vector ID
         --  encodes the priority of the protected object.

         if Prio /= Priority_Of_Interrupt (Interrupt) then
            raise Program_Error with
              "protected object for interrupt " & Interrupt'Image & " " &
              "requires aspect Interrupt_Priority => " &
              Priority_Of_Interrupt (Interrupt)'Image;
         end if;
      end Install_Interrupt_Handler;

      ---------------------------
      -- Priority_Of_Interrupt --
      ---------------------------

      function Priority_Of_Interrupt
        (Interrupt : System.BB.Interrupts.Interrupt_ID)
        return System.Any_Priority
      is
      begin
         --  Assert that it is a real interrupt

         pragma Assert (Interrupt /= System.BB.Interrupts.No_Interrupt);

         --  Convert the hardware priority to an Ada priority

         return System.Interrupt_Priority'First - 2 +
                  Priority_From_Interrupt_ID (Interrupt);
      end Priority_Of_Interrupt;

      ----------------
      -- Power_Down --
      ----------------

      procedure Power_Down is
      begin
         Asm ("hlt", Volatile => True);
      end Power_Down;

      -------------------------------------------------
      -- Set_Current_Priority -> Not (yet) supported --
      -------------------------------------------------

      procedure Set_Current_Priority (Priority : Integer) is null;
   end Interrupts;

   package body Time is
      --  For x86-64 we can generally determine the clock speed from the
      --  the information provided from processor. Consequently, we set the
      --  granularity of Ada.Real_Time to nanoseconds and convert between
      --  hardware and Ada.Real_Time time bases here in this package, similar
      --  to what is done in other operating systems.

      --  To facilitate the conversion between hardware and Ada.Real_Time time
      --  bases we use clock frequencies in kilohertz so we do not unduly cap
      --  the range of Ada.Real_Time.Time due to the overflow in the
      --  conversion.

      Ticks_Per_Millisecond : constant := Ticks_Per_Second / 1_000;

      ---------------------------
      -- Clear_Alarm_Interrupt --
      ---------------------------

      procedure Clear_Alarm_Interrupt is null;

      ---------------------------
      -- Install_Alarm_Handler --
      ---------------------------

      procedure Install_Alarm_Handler
        (Handler : BB.Interrupts.Interrupt_Handler) is
      begin
         BB.Interrupts.Attach_Handler
           (Handler,
            APIC_Timer_Vector,
            Interrupts.Priority_Of_Interrupt (APIC_Timer_Vector));
      end Install_Alarm_Handler;

      ----------------
      -- Read_Clock --
      ----------------

      function Read_Clock return BB.Time.Time is
      begin
         --  Read the raw clock and covert it to the Time time base
         return
           BB.Time.Time ((Sched_Info.TSC_Schedule_End * Ticks_Per_Millisecond)
                          / TSC_Frequency_In_kHz);
      end Read_Clock;

      ---------------
      -- Set_Alarm --
      ---------------

      procedure Set_Alarm (Ticks : BB.Time.Time) is
         use System.BB.Time;

         Now : constant BB.Time.Time := Read_Clock;
         --  The current time

         Time_To_Alarm : constant Unsigned_64 :=
           Unsigned_64 (if Ticks > Now then Ticks - Now else 0);
         --  Number of nanosecond Ticks until the timer needs to fire, or zero
         --  if the time is in the past.

         Timer_Value   : constant Unsigned_64 :=
           (Time_To_Alarm * APIC_Frequency_In_kHz) / Ticks_Per_Millisecond;
         --  The requested value of the timer
      begin
         Timed_Evt.TSC_Trigger_Value := Timer_Value;
      end Set_Alarm;
   end Time;

   package body Multiprocessors is separate;
end System.BB.Board_Support;
