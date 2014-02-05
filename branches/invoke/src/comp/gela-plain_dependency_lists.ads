with Gela.Compilation_Unit_Factories;
with Gela.Contexts;
with Gela.Dependency_Lists;
with Gela.Elements.Compilation_Units;
with Gela.Lexical_Types;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Doubly_Linked_Lists;

package Gela.Plain_Dependency_Lists is
   pragma Preelaborate;

   type Dependency_List (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Dependency_Lists.Dependency_List with private;

   type Dependency_List_Access is access all Dependency_List;

private

   type Unit_Kinds is (Unit_Declaration, Unit_Body, Subunit);

   type Unit_Index is record
      Kind : Unit_Kinds;
      Name : Gela.Lexical_Types.Symbol;
   end record;

   function Hash (Item : Unit_Index) return Ada.Containers.Hash_Type;

   package Unit_Index_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Unit_Index,
      Hash                => Hash,
      Equivalent_Elements => "=");

   type Unit_Data (Kind : Unit_Kinds := Unit_Body) is record
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access;

      case Kind is
         when Subunit =>
            Parent  : Gela.Lexical_Types.Symbol;
         when Unit_Body =>
            No_Spec : Boolean := False;
         when others =>
            null;
      end case;
   end record;

   package Unit_Data_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Unit_Data);

   type Dependency_List (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Dependency_Lists.Dependency_List with
   record
      Order     : Unit_Data_Lists.List;
      --  List of units with already resolved dependecy
      Ordered   : Unit_Index_Sets.Set;
      --  Index of Order list
      Queue     : Unit_Data_Lists.List;
      --  List of units with not yet resolved dependecy
      Queued    : Unit_Index_Sets.Set;
      --  Index of Queue list
   end record;

   overriding procedure Add_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access);

   overriding procedure Add_Body_Unit
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access);

   overriding procedure Add_Subunit
     (Self         : in out Dependency_List;
      Parent       : Gela.Lexical_Types.Symbol;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Units.Compilation_Unit_Access);

   overriding procedure Next_Required_Unit
     (Self         : in out Dependency_List;
      Name         : out Gela.Lexical_Types.Symbol;
      Declartion   : out Boolean);

   overriding procedure Create_Units
     (Self    : in out Dependency_List;
      Factory : Gela.Compilation_Unit_Factories.
        Compilation_Unit_Factory_Access);

end Gela.Plain_Dependency_Lists;
