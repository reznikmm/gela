--  This package provides Environment_Set.
with Gela.Contexts;
with Gela.Defining_Name_Cursors;
with Gela.Elements.Defining_Names;
with Gela.Environments;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Library_Environments;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Gela.Peristent_Lists;
with Gela.Name_List_Managers;

package Gela.Plain_Environments is
   pragma Preelaborate;

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with private;
   type Plain_Environment_Set_Access is access all Environment_Set;

private

   package Defining_Name_Lists is new Gela.Peristent_Lists
     (Element_Type => Gela.Elements.Defining_Names.Defining_Name_Access);

   subtype Defining_Name_Item_Count is Defining_Name_Lists.Count_Type;
   subtype Defining_Name_Item_Index is Defining_Name_Lists.Index_Type;

   --  Region_Item  --

   type Region_Item is record
      Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      --  Defining name corresponding to given region, if any
      Local : Gela.Name_List_Managers.List;
      --  List of Direct_Visible_Item.
      Use_Package : Defining_Name_Item_Count;
      --  List of use_package_clauses in the region
      Completion : Defining_Name_Item_Count;
      --  List of <Completion, Name> pairs
   end record;

   package Region_Item_Lists is new Gela.Peristent_Lists
     (Element_Type => Region_Item);

   subtype Region_Item_Count is Region_Item_Lists.Count_Type;
   subtype Region_Item_Index is Region_Item_Lists.Index_Type;

   --  Env_Item  --

   type Region_Enum is (Nested, Other, Withed);
   --  Nested - List of nested regions, current - first
   --  Other  - List of all visible regions except Nested_Region_List
   --  Withed - List of regions available over with clauses

   type Region_List_Array is array (Region_Enum) of Region_Item_Count;

   type Env_Item is record
      Region_List : Region_List_Array;
   end record;

   use type Gela.Semantic_Types.Env_Index;

   subtype Env_Item_Index is Gela.Semantic_Types.Env_Index
     range 1 .. Gela.Semantic_Types.Env_Index'Last - 1;
   --  Reserve last for Library_Level_Environment index

   package Env_Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Env_Item_Index,
      Element_Type => Env_Item);

   function Hash
     (X : Gela.Lexical_Types.Symbol) return Ada.Containers.Hash_Type;

   function Hash
     (X : Gela.Semantic_Types.Env_Index) return Ada.Containers.Hash_Type;

   package Symbol_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Lexical_Types.Symbol,
      Element_Type    => Gela.Semantic_Types.Env_Index,
      Hash            => Hash,
      Equivalent_Keys => Gela.Lexical_Types."=",
      "="             => Gela.Semantic_Types."=");

   package Env_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Semantic_Types.Env_Index,
      Element_Type    => Gela.Lexical_Types.Symbol,
      Hash            => Hash,
      Equivalent_Keys => Gela.Semantic_Types."=",
      "="             => Gela.Lexical_Types."=");

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with
   record
      Names : aliased Gela.Name_List_Managers.Name_List_Manager;
      Lib : aliased Gela.Library_Environments.Environment_Set (Context);
      Env            : Env_Item_Vectors.Vector;
      Region         : Region_Item_Lists.Container;
      Use_Package    : Defining_Name_Lists.Container;
      Units_Env      : Symbol_Maps.Map;
      --  Map of library level regions
      Lib_Env        : Env_Maps.Map;
      --  Reverse mapping for Units_Env
   end record;

   overriding function Empty_Environment
     (Self  : Environment_Set)
      return Gela.Semantic_Types.Env_Index;

   overriding function Add_With_Clause
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index;

   overriding function Add_Defining_Name
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Add_Use_Package
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Add_Completion
     (Self       : in out Environment_Set;
      Index      : Gela.Semantic_Types.Env_Index;
      Name       : Gela.Elements.Defining_Names.Defining_Name_Access;
      Completion : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Completions
     (Self       : in out Environment_Set;
      Index      : Gela.Semantic_Types.Env_Index;
      Name       : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Environments.Completion_List;

   overriding function Direct_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;

   overriding function Enter_Completion_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Enter_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Leave_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index)
      return Gela.Semantic_Types.Env_Index;

   overriding function Library_Level_Environment
     (Self  : Environment_Set)
      return Gela.Semantic_Types.Env_Index;

   overriding function Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index;

   overriding procedure Set_Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol;
      Value  : Gela.Semantic_Types.Env_Index);

   overriding function Use_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;

   overriding function Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : access Boolean)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;

end Gela.Plain_Environments;
