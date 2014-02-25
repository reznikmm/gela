--  This package provides trivial Interpretation_Manager

with Gela.Lexical_Types;
with Gela.Interpretations;
with Gela.Semantic_Types;

package Gela.Plain_Interpretations is
   pragma Preelaborate;

   type Interpretation_Manager is
     new Gela.Interpretations.Interpretation_Manager with private;

   type Interpretation_Manager_Access is
     access all Interpretation_Manager'Class;

private

   type Interpretation_Manager is
     new Gela.Interpretations.Interpretation_Manager with
   record
      null;
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

end Gela.Plain_Interpretations;
