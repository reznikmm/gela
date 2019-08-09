--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Characters.Handling;

package body Program.Units.Vectors is

   ------------
   -- Append --
   ------------

   procedure Append
     (Self  : in out Unit_Vector;
      Value : not null Program.Compilation_Units.Compilation_Unit_Access) is
   begin
      Self.Data.Append (Value);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Unit_Vector) is
   begin
      Self.Data.Clear;
   end Clear;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Unit_Vector;
      Index : Positive)
       return not null Program.Compilation_Units.Compilation_Unit_Access is
   begin
      return Self.Data (Index);
   end Element;

   ---------------
   -- Find_Unit --
   ---------------

   overriding function Find_Unit
     (Self : Unit_Vector;
      Name : Text) return Program.Compilation_Units.Compilation_Unit_Access
   is
      use Ada.Wide_Wide_Characters.Handling;
      Name_To_Lower : constant Text := To_Lower (Name);
   begin
      for J of Self.Data loop
         if To_Lower (J.Full_Name) = Name_To_Lower then
            return J;
         end if;
      end loop;

      return null;
   end Find_Unit;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (Self : Unit_Vector) return Positive is
   begin
      return Self.Data.Last_Index;
   end Get_Length;

end Program.Units.Vectors;
