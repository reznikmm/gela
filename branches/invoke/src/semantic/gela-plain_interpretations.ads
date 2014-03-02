--  This package provides trivial Interpretation_Manager

with Ada.Containers.Hashed_Maps;

with Gela.Elements.Defining_Names;
with Gela.Contexts;
with Gela.Lexical_Types;
with Gela.Interpretations;
with Gela.Semantic_Types;
with Gela.Solutions;
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
     (Key_Type        => Gela.Interpretations.Interpretation_Set_Index,
      Element_Type    => Gela.Int.Interpretation_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Interpretations."=",
      "="             => Gela.Int."=");

   package Solution_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Interpretations.Interpretation_Index,
      Element_Type    => Gela.Solutions.Solution_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Interpretations."=",
      "="             => Gela.Solutions."=");

   type Interpretation_Manager (Context : Gela.Contexts.Context_Access) is
     new Gela.Interpretations.Interpretation_Manager with
   record
      Interpretations : Int_Maps.Map;
      Solutions       : Solution_Maps.Map;
      Last_Int        : Gela.Interpretations.Interpretation_Set_Index := 0;
      Last_Solution   : Gela.Interpretations.Interpretation_Index := 0;
   end record;

   overriding procedure Direct_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Chosen_Interpretation
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index);

   overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access);

end Gela.Plain_Interpretations;
