with Gela.Int.Visiters;

package body Gela.Int.Expressions is

   ------------
   -- Create --
   ------------

   function Create
     (Down            : Gela.Interpretations.Interpretation_Index_Array;
      Expression_Type : Gela.Semantic_Types.Type_Index;
      Expression_Flag : Gela.Interpretations.Expression_Flags)
      return Expression is
   begin
      return (Index           => 0,
              Length          => Down'Length,
              Expression_Type => Expression_Type,
              Expression_Flag => Expression_Flag,
              Down            => Down);
   end Create;

   ---------------------
   -- Expression_Flag --
   ---------------------

   function Expression_Flag
     (Self : Expression)
      return Gela.Interpretations.Expression_Flags is
   begin
      return Self.Expression_Flag;
   end Expression_Flag;

   ---------------------
   -- Expression_Type --
   ---------------------

   function Expression_Type
     (Self : Expression)
      return Gela.Semantic_Types.Type_Index is
   begin
      return Self.Expression_Type;
   end Expression_Type;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Expression;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Expression (Self);
   end Visit;

end Gela.Int.Expressions;
