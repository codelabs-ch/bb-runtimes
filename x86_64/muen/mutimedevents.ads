--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

with Interfaces;

--  The Muen timed events mechanism implements a synthetic timer which can be
--  used by subjects to trigger events when a specified timestamp has passed.
--  This package contains declarations for the timed event data structures.
package Mutimedevents
is
   pragma Preelaborate;

   Event_Bits                 : constant := 6;
   Padding_Bits               : constant := 64 - Event_Bits;
   Timed_Event_Interface_Size : constant := 128;

   type Unsigned_6 is mod 2 ** Event_Bits
     with
       Size => Event_Bits;

   type Padding_Type is mod 2 ** Padding_Bits
     with
       Size => Padding_Bits;

   --  Timed events enable subjects to trigger events at a given time in the
   --  future.
   type Timed_Event_Interface_Type is record
      --  CPU tick count to fire the event designated by the event number
      --  field.
      TSC_Trigger_Value : Interfaces.Unsigned_64;
      --  Number of event to trigger.
      Event_Nr          : Unsigned_6;
      Padding           : Padding_Type;
   end record
     with
       Size        => Timed_Event_Interface_Size,
       Object_Size => Timed_Event_Interface_Size;

   for Timed_Event_Interface_Type use record
      TSC_Trigger_Value at 0 range 0 .. 63;
      Event_Nr          at 8 range 0 ..  Event_Bits - 1;
      Padding           at 8 range Event_Bits .. 63;
   end record;

end Mutimedevents;
