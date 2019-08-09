--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
--  Trivial implementation of subunits

with Program.Subunits;
with Program.Compilation_Unit_Vectors;
with Program.Units.Vectors;

package Program.Units.Subunits is
   pragma Preelaborate;

   type Subunit is new Unit and Program.Subunits.Subunit with private;

   procedure Initialize
     (Self             : in out Subunit;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Unit_Declaration : not null Program.Elements.Element_Access;
      Parent_Body      : not null Program.Compilation_Units
                           .Compilation_Unit_Access);

   procedure Append_Subunit
     (Self  : in out Subunit;
      Value : Program.Compilation_Units.Compilation_Unit_Access);

private
   type Subunit is new Unit and Program.Subunits.Subunit with record
      Subunits : aliased Program.Units.Vectors.Unit_Vector;

      Parent_Body : Program.Compilation_Units.Compilation_Unit_Access;
   end record;

   overriding function Subunits (Self : access Subunit)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;

   overriding function Parent_Body (Self : access Subunit)
     return not null Program.Compilation_Units.Compilation_Unit_Access;

end Program.Units.Subunits;
