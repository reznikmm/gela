--  This package provides Interpretation_Manager interface and its methods.
with Gela.Elements.Defining_Names;
with Gela.Lexical_Types;
with Gela.Semantic_Types;

package Gela.Interpretations is
   pragma Preelaborate;

   type Interpretation_Set_Index is new Natural;
   --  Index of set of interpretation inside an instance of manager

   type Interpretation_Index is new Natural;
   --  Index of interpretation inside an instance of manager

   type Interpretation_Manager is limited interface;
   --  This object keeps sets of possible interpretations
   type Interpretation_Manager_Access is
     access all Interpretation_Manager'Class;
   for Interpretation_Manager_Access'Storage_Size use 0;

   not overriding procedure Direct_Name
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Gela.Interpretations.Interpretation_Set_Index) is abstract;
   --  Return interpretations of Name symbol

   not overriding procedure Join_Selected_Component
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Gela.Interpretations.Interpretation_Set_Index) is abstract;
   --  Return interpretations of Prefix.Name symbol

   not overriding procedure Split_Selected_Component
     (Self     : in out Interpretation_Manager;
      Value    : Gela.Interpretations.Interpretation_Index;
      Prefix   : out Gela.Interpretations.Interpretation_Index;
      Selector : out Gela.Interpretations.Interpretation_Index) is abstract;
   --  For given interpretation of selected_component get interpretations of
   --  its profix and selector

   not overriding procedure Chosen_Interpretation
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index) is abstract;
   --  Return chosen interpretation from Set of a complete context

   not overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access)
        is abstract;
   --  Return defining name from interpretation

end Gela.Interpretations;
