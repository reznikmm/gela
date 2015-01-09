with Ada.Containers.Hashed_Maps;

with League.Strings;

with Gela.Contexts;
with Gela.Compilation_Units;
with Gela.Compilation_Managers;
with Gela.Compilation_Unit_Factories;
with Gela.Dependency_Lists;
with Gela.Lexical_Types;

package Gela.Plain_Compilation_Managers is
   pragma Preelaborate;

   type Compilation_Manager
     (Context : Gela.Contexts.Context_Access;
      Factory : Gela.Compilation_Unit_Factories.
        Compilation_Unit_Factory_Access) is
     limited new Gela.Compilation_Managers.Compilation_Manager with private;

   type Compilation_Manager_Access is access all Compilation_Manager;

   not overriding procedure Initialize
     (Self  : in out Compilation_Manager;
      Debug : League.Strings.Universal_String);

private

   function Hash
     (Item : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type;
   --  Support hashing of symbols

   package Package_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Lexical_Types.Symbol,
      Element_Type    => Gela.Compilation_Units.Package_Unit_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Lexical_Types."=",
      "="             => Gela.Compilation_Units."=");

   package Body_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Lexical_Types.Symbol,
      Element_Type    => Gela.Compilation_Units.Body_Unit_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Lexical_Types."=",
      "="             => Gela.Compilation_Units."=");

   package Spec_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Lexical_Types.Symbol,
      Element_Type    =>
         Gela.Compilation_Units.Library_Unit_Declaration_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Lexical_Types."=",
      "="             => Gela.Compilation_Units."=");

   package Subunit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Lexical_Types.Symbol,
      Element_Type    => Gela.Compilation_Units.Subunit_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Lexical_Types."=",
      "="             => Gela.Compilation_Units."=");

   type Compilation_Manager
     (Context : Gela.Contexts.Context_Access;
      Factory : Gela.Compilation_Unit_Factories.
        Compilation_Unit_Factory_Access) is
     limited new Gela.Compilation_Managers.Compilation_Manager with
   record
      Packages  : Package_Maps.Map;
      --  Map of packages placed in Order list
      Bodies    : Body_Maps.Map;
      --  Map of unit bodies placed in Order list
      Specs     : Spec_Maps.Map;
      --  Map of unit declarations placed in Order list
      Subunits  : Subunit_Maps.Map;
      --  Map of subunits placed in Order list
      Debug : League.Strings.Universal_String;
      --  Flags for dump data from debugging
   end record;

   overriding function Context
     (Self : Compilation_Manager) return Gela.Contexts.Context_Access;

   overriding procedure Read_Compilation
     (Self   : in out Compilation_Manager;
      Name   : League.Strings.Universal_String);

   overriding procedure Read_Dependency
     (Self   : in out Compilation_Manager;
      List   : Gela.Dependency_Lists.Dependency_List_Access);

   overriding procedure Read_Declaration
     (Self   : in out Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol);

   overriding procedure Read_Body
     (Self   : in out Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol);

   not overriding procedure Create_Unit
     (Self    : in out Compilation_Manager;
      Item    : Gela.Dependency_Lists.Unit_Data);

end Gela.Plain_Compilation_Managers;
