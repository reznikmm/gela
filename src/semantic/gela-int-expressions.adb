with Gela.Int.Visiters;

package body Gela.Int.Expressions is

   ------------
   -- Create --
   ------------

   function Create
     (Down            : Gela.Interpretations.Interpretation_Index_Array;
      Expression_Type : Gela.Semantic_Types.Type_Index;
      Expression_Kind : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds)
      return Expression is
   begin
      return (Index           => 0,
              Length          => Down'Length,
              Expression_Type => Expression_Type,
              Expression_Kind => Expression_Kind,
              Down            => Down);
   end Create;

   ---------------------
   -- Expression_Kind --
   ---------------------

   function Expression_Kind
     (Self : Expression)
      return Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds is
   begin
      return Self.Expression_Kind;
   end Expression_Kind;

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
