with Gela.Int.Visiters;

package body Gela.Int.Expressions is

   ------------
   -- Create --
   ------------

   function Create
     (Expression_Type : Gela.Semantic_Types.Type_Index;
      Save : Gela.Saves.Save_Access)
      return Expression is
   begin
      return (Expression_Type, Save);
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

   ----------
   -- Save --
   ----------

   overriding function Save
     (Self : Expression) return Gela.Saves.Save_Access is
   begin
      return Self.Save;
   end Save;

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
