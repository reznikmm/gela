with Gela.Int.Visiters;

package body Gela.Int.Tuples is

   ------------
   -- Create --
   ------------

   function Create
     (Value : Gela.Interpretations.Interpretation_Set_Index_Array)
      return Tuple is
   begin
      return (Index  => 0,
              Length => 0,
              Value  => Value,
              Size   => Value'Length,
              Down   => (others => 0));
   end Create;

   -----------
   -- Value --
   -----------

   function Value
     (Self : Tuple) return Gela.Interpretations.Interpretation_Set_Index_Array
   is
   begin
      return Self.Value;
   end Value;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Tuple;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Tuple (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Chosen_Tuple;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Chosen_Tuple (Self);
   end Visit;

end Gela.Int.Tuples;
