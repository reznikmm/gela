with Gela.Int.Visiters;

package body Gela.Int.Expressions is

   ------------
   -- Create --
   ------------

   function Create
     (Children        : Natural;
      Expression_Type : Gela.Semantic_Types.Type_Index)
      return Expression is
   begin
      return (Length          => Children,
              Expression_Type => Expression_Type,
              Down            => (others => 0));
   end Create;

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
