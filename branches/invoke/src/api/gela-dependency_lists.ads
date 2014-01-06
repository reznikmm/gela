with Gela.Compilation_Unit_Fabrics;
with Gela.Elements.Compilation_Units;
with Gela.Lexical_Types;

package Gela.Dependency_Lists is
   pragma Preelaborate;

   type Dependency_List is limited interface;
   type Dependency_List_Access is
     access all Dependency_List'Class;
   for Dependency_List_Access'Storage_Size use 0;

   not overriding procedure Add_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
   is abstract;

   not overriding procedure Add_Body_Unit
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
   is abstract;

   not overriding procedure Add_Subunit
     (Self         : in out Dependency_List;
      Parent       : Gela.Lexical_Types.Symbol;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
   is abstract;

   not overriding procedure Next_Required_Unit
     (Self         : in out Dependency_List;
      Name         : out Gela.Lexical_Types.Symbol;
      Declartion   : out Boolean)
   is abstract;

   not overriding procedure Create_Units
     (Self   : in out Dependency_List;
      Fabric : Gela.Compilation_Unit_Fabrics.Compilation_Unit_Fabric_Access)
   is abstract;

end Gela.Dependency_Lists;
