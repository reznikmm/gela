with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;

with Gela.Contexts;
with Gela.Dependency_Lists;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Subunits;
with Gela.Lexical_Types;

package Gela.Plain_Dependency_Lists is
   pragma Preelaborate;

   type Dependency_List (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Dependency_Lists.Dependency_List with private;

   type Dependency_List_Access is access all Dependency_List;

private

   subtype Unit_Kinds is Gela.Dependency_Lists.Unit_Kinds;
   use all type Gela.Dependency_Lists.Unit_Kinds;

   type Unit_Index is record
      Kind : Unit_Kinds;
      Name : Gela.Lexical_Types.Symbol;
   end record;

   function Hash (Item : Unit_Index) return Ada.Containers.Hash_Type;

   package Unit_Index_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Unit_Index,
      Hash                => Hash,
      Equivalent_Elements => "=");

   package Unit_Data_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Gela.Dependency_Lists.Unit_Data, Gela.Dependency_Lists."=");

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
      Pending   : Unit_Index_Sets.Set;
      --  Set of requested units over Next_Required_Unit call
      No_Spec   : Unit_Index_Sets.Set;
      --  Set of absent subprogram units over Next_Required_Unit call
   end record;

   overriding procedure Add_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access);

   overriding procedure No_Library_Unit_Declaration
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol);

   overriding procedure Add_Body_Unit
     (Self         : in out Dependency_List;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access);

   overriding procedure Add_Subunit
     (Self         : in out Dependency_List;
      Parent       : Gela.Lexical_Types.Symbol;
      Name         : Gela.Lexical_Types.Symbol;
      Withed       : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List;
      Unit         : Gela.Elements.Subunits.Subunit_Access);

   overriding procedure Next_Action
     (Self   : in out Dependency_List;
      Action : out Gela.Dependency_Lists.Action);

end Gela.Plain_Dependency_Lists;
