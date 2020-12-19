--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------
--  Trivial implementation of library bodies

with Program.Library_Unit_Bodies;
with Program.Library_Unit_Declarations;
with Program.Compilation_Unit_Vectors;
with Program.Units.Vectors;

package Program.Units.Bodies is
   pragma Preelaborate;

   type Unit_Body is new Unit
     and Program.Library_Unit_Bodies.Library_Unit_Body with private;

   procedure Initialize
     (Self             : in out Unit_Body;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Unit_Declaration : not null Program.Elements.Element_Access;
      Parent           : Program.Library_Unit_Declarations
                           .Library_Unit_Declaration_Access;
      Declaration      : Program.Library_Unit_Declarations
                           .Library_Unit_Declaration_Access);

   procedure Append_Subunit
     (Self  : in out Unit_Body;
      Value : Program.Compilation_Units.Compilation_Unit_Access);

private
   type Unit_Body is new Unit
     and Program.Library_Unit_Bodies.Library_Unit_Body with
   record
      Parent : Program.Library_Unit_Declarations
                 .Library_Unit_Declaration_Access;

      Declaration : Program.Library_Unit_Declarations
                      .Library_Unit_Declaration_Access;

      Subunits : aliased Program.Units.Vectors.Unit_Vector;
   end record;

   overriding function Parent (Self : access Unit_Body)
     return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access;

   overriding function Corresponding_Declaration (Self : access Unit_Body)
     return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access;

   overriding function Subunits (Self : access Unit_Body)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;

   overriding function Is_Library_Unit_Body_Unit
     (Self : Unit_Body) return Boolean;

end Program.Units.Bodies;
