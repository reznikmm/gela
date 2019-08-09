--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
--  Trivial implementation of library declarations

with Program.Library_Unit_Bodies;
with Program.Library_Unit_Declarations;
with Program.Compilation_Unit_Vectors;
with Program.Units.Vectors;

package Program.Units.Declarations is
   pragma Preelaborate;

   type Unit_Declaration is new Unit
     and Program.Library_Unit_Declarations.Library_Unit_Declaration
       with private;

   procedure Initialize
     (Self             : in out Unit_Declaration;
      Compilation      : Program.Compilations.Compilation_Access;
      Full_Name        : Text;
      Context_Clause   : Program.Element_Vectors.Element_Vector_Access;
      Declaration      : not null Program.Elements.Element_Access;
      Parent           : Program.Library_Unit_Declarations
                           .Library_Unit_Declaration_Access);

   procedure Append_Child
     (Self  : in out Unit_Declaration;
      Value : Program.Compilation_Units.Compilation_Unit_Access);

   procedure Set_Body
     (Self  : in out Unit_Declaration;
      Value : Program.Library_Unit_Bodies.Library_Unit_Body_Access);

private
   type Unit_Declaration is new Unit
     and Program.Library_Unit_Declarations.Library_Unit_Declaration
       with
   record
      Parent : Program.Library_Unit_Declarations
                 .Library_Unit_Declaration_Access;

      Impl : Program.Library_Unit_Bodies.Library_Unit_Body_Access;

      Childern : aliased Program.Units.Vectors.Unit_Vector;
   end record;

   overriding function Parent (Self : access Unit_Declaration)
     return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access;

   overriding function Corresponding_Body (Self : access Unit_Declaration)
     return Program.Library_Unit_Bodies.Library_Unit_Body_Access;

   overriding function Corresponding_Childern
     (Self : access Unit_Declaration)
       return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;

end Program.Units.Declarations;
