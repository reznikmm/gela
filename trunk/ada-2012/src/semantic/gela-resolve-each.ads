with Gela.Type_Managers;

private package Gela.Resolve.Each is
   pragma Preelaborate;

   function Expression
     (Self : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Expression_Iterators.Forward_Iterator'Class;
   --  Resolve given interpretation set as expression. So ignore symbol and
   --  others non-expression interpretations. Translate defining name into
   --  expression.

   function Prefix
     (Self : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Expression_Iterators.Forward_Iterator'Class;
   --  The same as Expression, but add implicit dereference interpretations.

end Gela.Resolve.Each;
