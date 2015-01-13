--  Type manager keeps types found in all compilation units.

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Hashed_Maps;

with Gela.Contexts;
with Gela.Elements.Defining_Names;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Subtype_Marks;
with Gela.Semantic_Types;
with Gela.Type_Managers;
with Gela.Type_Views;

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
        (Key_Type     => Gela.Semantic_Types.Type_Index,
         Element_Type => Gela.Type_Views.Type_View_Access,
         "<"          => Gela.Semantic_Types."<",
         "="          => Gela.Type_Views."=");

   type Back_Key is record
      Category : Gela.Type_Views.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access;
   end record;

   function Hash (Key : Back_Key) return Ada.Containers.Hash_Type;

   package Back_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Back_Key,
      Element_Type    => Gela.Semantic_Types.Type_Index,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Gela.Semantic_Types."=");

   type Type_Manager (Context : Gela.Contexts.Context_Access) is
     new Gela.Type_Managers.Type_Manager with
   record
       Map  : Type_View_Maps.Map;
       Back : Back_Maps.Map;
   end record;

   not overriding function Get
     (Self     : access Type_Manager;
      Category : Gela.Type_Views.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
        return Gela.Semantic_Types.Type_Index;

   overriding function Get
     (Self  : access Type_Manager;
      Index : Gela.Semantic_Types.Type_Index)
      return Gela.Type_Views.Type_View_Access;

   overriding function Type_From_Declaration
     (Self  : access Type_Manager;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_Index;

   overriding function Type_From_Subtype_Mark
     (Self  : access Type_Manager;
      Node  : Gela.Elements.Subtype_Marks.Subtype_Mark_Access)
      return Gela.Semantic_Types.Type_Index;

   overriding function Type_By_Name
     (Self  : access Type_Manager;
      Node  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Type_Index;

   overriding function Universal_Integer
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_Index;

   overriding function Universal_Real
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_Index;

   overriding function Universal_Access
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_Index;

end Gela.Plain_Type_Managers;
