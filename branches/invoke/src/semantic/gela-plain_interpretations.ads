--  This package provides trivial Interpretation_Manager

with Ada.Containers.Hashed_Maps;

with Gela.Elements.Defining_Names;
with Gela.Contexts;
with Gela.Interpretations;
with Gela.Semantic_Types;
with Gela.Int;

package Gela.Plain_Interpretations is
   pragma Preelaborate;

   type Interpretation_Manager (Context : Gela.Contexts.Context_Access) is
     new Gela.Interpretations.Interpretation_Manager with private;

   type Interpretation_Manager_Access is
     access all Interpretation_Manager'Class;

private

   function Hash
     (Value : Gela.Interpretations.Interpretation_Set_Index)
      return Ada.Containers.Hash_Type;

   function Hash
     (Value : Gela.Interpretations.Interpretation_Index)
      return Ada.Containers.Hash_Type;

   package Int_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Interpretations.Interpretation_Index,
      Element_Type    => Gela.Int.Interpretation_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Interpretations."=",
      "="             => Gela.Int."=");

   type Interpretation_Manager (Context : Gela.Contexts.Context_Access) is
     new Gela.Interpretations.Interpretation_Manager with
   record
      Interpretations : Int_Maps.Map;
      Last_Int        : Gela.Interpretations.Interpretation_Index := 0;
   end record;

   overriding procedure Add_Defining_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Expression
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_Index;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Target : in out Gela.Interpretations.Visiter'Class);
   --  Iterate over all interpretations in Set and call Target visiter

   overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Index;
      Target : in out Gela.Interpretations.Visiter'Class);
   --  For given interpretations call Target visiter

   overriding procedure Get_Down_Interpretation
     (Self     : in out Interpretation_Manager;
      Value    : Gela.Interpretations.Interpretation_Index;
      Index    : Positive;
      Result   : out Gela.Interpretations.Interpretation_Index);
   --  Return interpretation from which Value was derived

   overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access);

end Gela.Plain_Interpretations;
