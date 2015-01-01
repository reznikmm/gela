with Gela.Int.Visiters;

package body Gela.Int.Placeholders is

   ------------
   -- Create --
   ------------

   function Create
     (Down : Gela.Interpretations.Interpretation_Index_Array;
      Kind : Gela.Interpretations.Placeholder_Kind)
      return Placeholder is
   begin
      return (Index            => 0,
              Length           => Down'Length,
              Placeholder_Kind => Kind,
              Down             => Down);
   end Create;

   ---------------------
   -- Placeholder_Kind --
   ---------------------

   function Placeholder_Kind
     (Self : Placeholder)
      return Gela.Interpretations.Placeholder_Kind is
   begin
      return Self.Placeholder_Kind;
   end Placeholder_Kind;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Placeholder;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Placeholder (Self);
   end Visit;

end Gela.Int.Placeholders;
