--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
--
--  Trivial implementation compilation unit base class

with Ada.Strings.Wide_Wide_Unbounded;

with Program.Compilation_Units;
with Program.Compilations;
with Program.Elements;
with Program.Element_Vectors;

private
package Program.Units is
   pragma Preelaborate;

   type Unit is abstract limited new Program.Compilation_Units.Compilation_Unit
     with private;

   procedure Initialize
     (Self             : in out Unit'Class;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Unit_Declaration : not null Program.Elements.Element_Access);

private

   type Unit is abstract limited new Program.Compilation_Units.Compilation_Unit
     with
   record
      Compilation : Program.Compilations.Compilation_Access;

      Full_Name : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

      Context_Clause : Program.Element_Vectors.Element_Vector_Access;

      Unit_Declaration : Program.Elements.Element_Access;

   end record;

   overriding function Compilation (Self : access Unit)
     return Program.Compilations.Compilation_Access;

   overriding function Full_Name (Self : access Unit) return Text;

   overriding function Context_Clause_Elements (Self : access Unit)
     return Program.Element_Vectors.Element_Vector_Access;

   overriding function Unit_Declaration (Self : access Unit)
     return not null Program.Elements.Element_Access;

   overriding function Is_Subunit_Unit (Self : Unit) return Boolean;
   overriding function Is_Library_Item_Unit (Self : Unit) return Boolean;
   overriding function Is_Library_Unit_Body_Unit (Self : Unit) return Boolean;

   overriding function Is_Library_Unit_Declaration_Unit
     (Self : Unit) return Boolean;

end Program.Units;
