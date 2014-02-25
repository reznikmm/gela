--  This package provides Interpretation_Manager interface and its methods.

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
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Interpretation_Set_Index) is abstract;
   --  Return interpretation of Name symbol

   not overriding procedure Chosen_Interpretation
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Interpretation_Set_Index;
      Result : out Interpretation_Index) is abstract;
   --  Return chosen interpretation from Set of a complete context

end Gela.Interpretations;
