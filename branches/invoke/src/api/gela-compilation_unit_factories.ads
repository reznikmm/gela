with Gela.Compilation_Units;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
--  with Gela.Elements.Compilation_Units;
with Gela.Elements.Subunits;
with Gela.Lexical_Types;

package Gela.Compilation_Unit_Factories is
   pragma Preelaborate;

   type Compilation_Unit_Factory is limited interface;
   type Compilation_Unit_Factory_Access is
     access all Compilation_Unit_Factory'Class;
   for Compilation_Unit_Factory_Access'Storage_Size use 0;

   not overriding function Create_Library_Unit_Declaration
     (Self   : in out Compilation_Unit_Factory;
      Parent : Gela.Compilation_Units.Package_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access
        is abstract;

   not overriding function Create_Body_Unit_Without_Declaration
     (Self   : in out Compilation_Unit_Factory;
      Parent : Gela.Compilation_Units.Package_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
      return Gela.Compilation_Units.Body_Unit_Access is abstract;

   not overriding function Create_Body_Unit
     (Self        : in out Compilation_Unit_Factory;
      Declaration : Gela.Compilation_Units.Library_Unit_Declaration_Access;
      Name        : Gela.Lexical_Types.Symbol;
      Node        : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
      return Gela.Compilation_Units.Body_Unit_Access is abstract;

   not overriding function Create_Subunit
     (Self   : in out Compilation_Unit_Factory;
      Parent : Gela.Compilation_Units.Body_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node        : Gela.Elements.Subunits.Subunit_Access)
      return Gela.Compilation_Units.Subunit_Access is abstract;

   not overriding function Create_Subunit
     (Self   : in out Compilation_Unit_Factory;
      Parent : Gela.Compilation_Units.Subunit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Subunits.Subunit_Access)
      return Gela.Compilation_Units.Subunit_Access is abstract;

end Gela.Compilation_Unit_Factories;
