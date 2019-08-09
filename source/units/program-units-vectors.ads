--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;

with Program.Compilation_Unit_Vectors;

package Program.Units.Vectors is
   pragma Preelaborate;

   type Unit_Vector is limited new
     Program.Compilation_Unit_Vectors.Compilation_Unit_Vector with private;

   procedure Clear (Self : in out Unit_Vector);

   procedure Append
     (Self  : in out Unit_Vector;
      Value : not null Program.Compilation_Units.Compilation_Unit_Access);

private

   package Unit_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Program.Compilation_Units.Compilation_Unit_Access,
      "="          => Program.Compilation_Units."=");

   type Unit_Vector is limited new
     Program.Compilation_Unit_Vectors.Compilation_Unit_Vector with
   record
      Data : Unit_Vectors.Vector;
   end record;

   overriding function Get_Length (Self : Unit_Vector) return Positive;

   overriding function Element
     (Self  : Unit_Vector;
      Index : Positive)
        return not null Program.Compilation_Units.Compilation_Unit_Access;

   overriding function Find_Unit
     (Self  : Unit_Vector;
      Name  : Text) return Program.Compilation_Units.Compilation_Unit_Access;

end Program.Units.Vectors;
