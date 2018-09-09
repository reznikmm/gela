--  Type manager keeps types found in all compilation units.

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Hashed_Maps;

with Gela.Contexts;
with Gela.Elements.Defining_Names;
with Gela.Elements.Discrete_Subtype_Definitions;
with Gela.Elements.Formal_Type_Declarations;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Object_Definitions;
with Gela.Elements.Root_Type_Definitions;
with Gela.Elements.Subtype_Mark_Or_Access_Definitions;
with Gela.Lexical_Types;
with Gela.Profiles;
with Gela.Semantic_Types;
with Gela.Type_Categories;
with Gela.Type_Managers;
with Gela.Types;
with Gela.Types.Simple;

package Gela.Plain_Type_Managers is
   pragma Preelaborate;

   type Type_Manager (Context : Gela.Contexts.Context_Access) is
     new Gela.Type_Managers.Type_Manager with private;

   type Type_Manager_Access is access all Type_Manager'Class;

   procedure Initialize
     (Self     : access Type_Manager;
      Standard : Gela.Elements.Element_Access);

private

   package Type_View_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Gela.Semantic_Types.Type_View_Index,
         Element_Type => Gela.Type_Categories.Type_View_Access,
         "<"          => Gela.Semantic_Types."<",
         "="          => Gela.Type_Categories."=");

   type Back_Key is record
      Category : Gela.Type_Categories.Category_Kinds;
      Decl     : access Gela.Elements.Element'Class;
   end record;

   function Hash (Key : Back_Key) return Ada.Containers.Hash_Type;

   package Back_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Back_Key,
      Element_Type    => Gela.Semantic_Types.Type_View_Index,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Gela.Semantic_Types."=");

   function Hash
     (Self : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Ada.Containers.Hash_Type;

   type Profile_Access is access all Gela.Profiles.Profile'Class;

   package Profile_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Elements.Defining_Names.Defining_Name_Access,
      Element_Type    => Profile_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Elements.Defining_Names."=");

   type Attribute_Key is record
      Tipe      : Gela.Semantic_Types.Type_View_Index;
      Attribute : Gela.Lexical_Types.Symbol;
   end record;

   function Hash (Value : Attribute_Key) return Ada.Containers.Hash_Type;

   package Attribute_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Attribute_Key,
      Element_Type    => Profile_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   function Hash
     (Self : Gela.Elements.Root_Type_Definitions.
        Root_Type_Definition_Access)
      return Ada.Containers.Hash_Type;

   package Root_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Elements.Root_Type_Definitions.
        Root_Type_Definition_Access,
      Element_Type    => Gela.Semantic_Types.Type_View_Index,
      Hash            => Hash,
      Equivalent_Keys => Gela.Elements.Root_Type_Definitions."=",
      "="             => Gela.Semantic_Types."=");

   type Type_Manager (Context : Gela.Contexts.Context_Access) is
     new Gela.Type_Managers.Type_Manager with
   record
       Map        : Type_View_Maps.Map;
       Back       : Back_Maps.Map;
       Profiles   : Profile_Maps.Map;
       Attributes : Attribute_Maps.Map;
       Roots      : Root_Maps.Map;
       Boolean    : Gela.Elements.Defining_Names.Defining_Name_Access;
   end record;

   not overriding function Get
     (Self     : access Type_Manager;
      Category : Gela.Type_Categories.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
        return Gela.Semantic_Types.Type_View_Index;

   not overriding function Get
     (Self     : access Type_Manager;
      Category : Gela.Type_Categories.Category_Kinds;
      Decl     : Gela.Elements.Formal_Type_Declarations
      .Formal_Type_Declaration_Access)
        return Gela.Semantic_Types.Type_View_Index;

   not overriding function Get_Derived
     (Self     : access Type_Manager;
      Parent   : Gela.Type_Categories.Type_View_Access;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
      return Gela.Semantic_Types.Type_View_Index;

   not overriding function Get_Array
     (Self      : access Type_Manager;
      Category  : Gela.Type_Categories.Category_Kinds;
      Decl      : Gela.Elements.Full_Type_Declarations
                    .Full_Type_Declaration_Access;
      Component : Gela.Semantic_Types.Type_View_Index;
      Indexes   : Gela.Types.Simple.Discrete_Type_Array)
      return Gela.Semantic_Types.Type_View_Index;

   overriding function Get
     (Self  : access Type_Manager;
      Index : Gela.Semantic_Types.Type_View_Index)
      return Gela.Types.Type_View_Access;

   overriding function Type_From_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_View_Index;

   overriding function Type_Of_Object_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_View_Index;

   overriding function Type_From_Subtype_Indication
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Object_Definitions.Object_Definition'Class)
      return Gela.Semantic_Types.Type_View_Index;

   overriding function Type_From_Subtype_Mark
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Subtype_Mark_Or_Access_Definitions.
                Subtype_Mark_Or_Access_Definition'Class)
      return Gela.Semantic_Types.Type_View_Index;

   overriding function Type_From_Discrete_Subtype
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Discrete_Subtype_Definitions.
                Discrete_Subtype_Definition'Class)
        return Gela.Semantic_Types.Type_View_Index;

   overriding function Type_By_Name
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Type_View_Index;

   overriding function Universal_Integer
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index;

   overriding function Universal_Real
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index;

   overriding function Universal_Access
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index;

   overriding function Root_Integer
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index;

   overriding function Root_Real
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index;

   overriding function Get_Profile
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Profiles.Profile_Access;

   overriding function Get_Profile
     (Self      : access Type_Manager;
      Tipe      : Gela.Semantic_Types.Type_View_Index;
      Attribute : Gela.Lexical_Types.Symbol)
         return Gela.Profiles.Profile_Access;

   overriding function Boolean
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index;

end Gela.Plain_Type_Managers;
