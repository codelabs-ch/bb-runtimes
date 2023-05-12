------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                    Copyright (C) 2023, codelabs GmbH                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

with Musinfo.Instance;

procedure Hello is
   Timeout : Time               := Clock;
   Cycle   : constant Time_Span := Seconds (1);
   Name    : Musinfo.Name_Type;
begin
   if not Musinfo.Instance.Is_Valid then
      raise Program_Error with "Sinfo not valid";
   end if;

   Put_Line ("Sinfo is valid");
   Name := Musinfo.Instance.Subject_Name;

   Put_Line ("Subject name : " & String (Name.Data));
   Put_Line ("TSC kHZ :"  & Musinfo.Instance.TSC_Khz'Img);

   for I in Positive range 1 .. 5 loop
      delay until Timeout;
      Timeout := Timeout + Cycle;
      Put_Line ("Hello Muen" & I'Img);
   end loop;

   Put_Line ("Raising exception");
   raise Constraint_Error with "test";
end Hello;
