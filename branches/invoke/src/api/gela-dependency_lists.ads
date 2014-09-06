--  This package provides Dependency_List interface and their methods.

with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Subunits;
with Gela.Lexical_Types;

package Gela.Dependency_Lists is
   pragma Preelaborate;

   type Dependency_List is limited interface;
   --  This type resolves semantic dependency between compilation units.
   --  User provides initial unit(s) with names and with lists, then
   --  read action needed to satisfy dependecny.

   type Dependency_List_Access is
     access all Dependency_List'Class;
   for Dependency_List_Access'Storage_Size use 0;

   not overriding procedure Add_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access)
   is abstract;
   --  Provide library unit declaration

   not overriding procedure No_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol) is abstract;
   --  Provide no library unit declaration exist

   not overriding procedure Add_Body_Unit
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
   is abstract;
   --  Provide body unit

   not overriding procedure Add_Subunit
     (Self         : in out Dependency_List;
      Parent       : Gela.Lexical_Types.Symbol;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Subunits.Subunit_Access)
   is abstract;
   --  Provide subunit

   type Unit_Kinds is (Unit_Declaration, Unit_Body, Subunit);

   type Unit_Data (Kind : Unit_Kinds := Unit_Body) is record
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;

      case Kind is
         when Subunit =>
            Parent  : Gela.Lexical_Types.Symbol;
            Subunit : Gela.Elements.Subunits.Subunit_Access;
         when Unit_Body =>
            Unit_Body : Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access;
         when Unit_Declaration =>
            Unit_Declaration : Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access;
            Is_Package       : Boolean;
      end case;
   end record;

   type Action_Kinds is (Unit_Ready, Unit_Required, Complete);

   type Action (Action_Kind : Action_Kinds := Unit_Ready) is record
      case Action_Kind is
         when Unit_Ready =>
            Unit : Unit_Data;

         when Unit_Required =>
            Full_Name : Gela.Lexical_Types.Symbol;
            Unit_Kind : Unit_Kinds := Unit_Declaration;

         when Complete =>
            null;
      end case;
   end record;

   not overriding procedure Add_Compilation_Unit
     (Self  : in out Dependency_List;
      Value : Unit_Data) is abstract;
   --  Provide compilation unit

   not overriding procedure Next_Action
     (Self   : in out Dependency_List;
      Action : out Gela.Dependency_Lists.Action)
   is abstract;
   --  Get next action of resolving dependency

end Gela.Dependency_Lists;
