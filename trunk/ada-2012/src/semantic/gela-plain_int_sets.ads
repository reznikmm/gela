with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;

with Gela.Int;
with Gela.Int_Sets;
with Gela.Interpretations;

package Gela.Plain_Int_Sets is
   pragma Preelaborate;

   type Interpretation_Set (Ids : access Gela.Int_Sets.Index_Provider'Class) is
     new Gela.Int_Sets.Interpretation_Set with private;

   type Interpretation_Set_Access is access all Interpretation_Set'Class;

   not overriding procedure Add
     (Self  : access Interpretation_Set;
      Index : in out Gela.Interpretations.Interpretation_Set_Index;
      Item  : Gela.Int.Interpretation_Access);

   not overriding procedure Add
     (Self  : access Interpretation_Set;
      Index : out Gela.Interpretations.Interpretation_Index;
      Item  : Gela.Int.Interpretation_Access);

private

   package Int_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Gela.Int.Interpretation_Access,
      "="          => Gela.Int."=");

   function Hash
     (Value : Gela.Interpretations.Interpretation_Set_Index)
      return Ada.Containers.Hash_Type;

   package Int_List_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Interpretations.Interpretation_Set_Index,
      Element_Type    => Int_Lists.List,
      Hash            => Hash,
      Equivalent_Keys => Gela.Interpretations."=",
      "="             => Int_Lists."=");

   function Hash
     (Value : Gela.Interpretations.Interpretation_Index)
      return Ada.Containers.Hash_Type;

   package Int_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Interpretations.Interpretation_Index,
      Element_Type    => Gela.Int.Interpretation_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Interpretations."=",
      "="             => Gela.Int."=");

   type Interpretation_Set (Ids : access Gela.Int_Sets.Index_Provider'Class) is
     new Gela.Int_Sets.Interpretation_Set with
   record
      Set_From, Set_To   : Gela.Interpretations.Interpretation_Set_Index := 0;
      Item_From, Item_To : Gela.Interpretations.Interpretation_Index := 0;
      Map                : Int_List_Maps.Map;
      Int_Map            : Int_Maps.Map;
   end record;

   overriding function Element
     (Self  : Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Index)
     return Gela.Int.Interpretation_Access;

   overriding function Symbols
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Symbol_Iterators
                 .Forward_Iterator'Class;

   overriding function Categories
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Category_Iterators
                 .Forward_Iterator'Class;

   overriding function Defining_Names
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Defining_Name_Iterators
                 .Forward_Iterator'Class;

   overriding function Expressions
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Expression_Iterators
                 .Forward_Iterator'Class;

   overriding function Profiles
     (Self  : access Interpretation_Set;
      Index : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Profile_Iterators
                 .Forward_Iterator'Class;

   overriding function Each
     (Self   : access Interpretation_Set;
      Index  : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Any_Iterators
                 .Forward_Iterator'Class;

end Gela.Plain_Int_Sets;
