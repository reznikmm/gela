with Gela.Compilation_Unit_Sets;
with Gela.Compilation_Units;
with Gela.Compilations;
with Gela.Contexts;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Units;
with Gela.Elements.Subunits;
with Gela.Lexical_Types;
with Gela.Plain_Compilation_Unit_Sets;
with Gela.Unit_Containers;

package Gela.Plain_Compilation_Units is
   pragma Preelaborate;

   type Compilation_Unit is limited new
     Gela.Compilation_Units.Compilation_Unit
     and Gela.Compilation_Units.Library_Item
     and Gela.Compilation_Units.Body_Unit
     and Gela.Compilation_Units.Library_Unit_Declaration
     and Gela.Compilation_Units.Package_Unit
     and Gela.Compilation_Units.Subunit
   with private;
   type Compilation_Unit_Access is access all Compilation_Unit'Class;

   function Create_Body
     (Node   : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Parent : Gela.Compilation_Units.Package_Unit_Access;
      Decl   : Gela.Compilation_Units.Library_Unit_Declaration_Access)
      return Compilation_Unit_Access;

   function Create_Declaration
     (Node   : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Parent : Gela.Compilation_Units.Package_Unit_Access)
      return Compilation_Unit_Access;

   function Create_Subunit
     (Node   : Gela.Elements.Subunits.Subunit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Parent : Compilation_Unit_Access)
      return Compilation_Unit_Access;

private

   type Compilation_Unit is limited new
     Gela.Compilation_Units.Compilation_Unit
     and Gela.Compilation_Units.Library_Item
     and Gela.Compilation_Units.Body_Unit
     and Gela.Compilation_Units.Library_Unit_Declaration
     and Gela.Compilation_Units.Package_Unit
     and Gela.Compilation_Units.Subunit
   with record
      Name        : Gela.Lexical_Types.Symbol;
      Container   : Gela.Unit_Containers.Unit_Container_Access;
      Compilation : Gela.Compilations.Compilation_Access;
      Parent      : Gela.Compilation_Units.Package_Unit_Access;
      Tree        : Gela.Elements.Compilation_Units.Compilation_Unit_Access;

      Corresponding_Declaration :
        Gela.Compilation_Units.Library_Unit_Declaration_Access;

      Subunits : aliased
        Gela.Plain_Compilation_Unit_Sets.Compilation_Unit_Set;

      Corresponding_Body : Gela.Compilation_Units.Body_Unit_Access;

      Corresponding_Childern : aliased
        Gela.Plain_Compilation_Unit_Sets.Compilation_Unit_Set;

      Corresponding_Subunit_Parent_Body :
        Gela.Compilation_Units.Compilation_Unit_Access;
   end record;

   overriding function Name
     (Self : access Compilation_Unit) return Gela.Lexical_Types.Symbol;

   overriding function Context
     (Self : access Compilation_Unit) return Gela.Contexts.Context_Access;

   overriding function Container
     (Self : access Compilation_Unit)
      return Gela.Unit_Containers.Unit_Container_Access;

   overriding function Compilation
     (Self : access Compilation_Unit)
      return Gela.Compilations.Compilation_Access;

   overriding function Parent
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Package_Unit_Access;

   overriding function Corresponding_Declaration
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access;

   overriding function Subunits
     (Self : access Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Corresponding_Body
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Body_Unit_Access;

   overriding function Corresponding_Childern
     (Self : access Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Corresponding_Subunit_Parent_Body
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Compilation_Unit_Access;

   overriding function Tree
     (Self : access Compilation_Unit)
      return Gela.Elements.Compilation_Units.Compilation_Unit_Access;

end Gela.Plain_Compilation_Units;
