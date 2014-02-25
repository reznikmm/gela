package body Gela.Plain_Interpretations is

   ---------------------------
   -- Chosen_Interpretation --
   ---------------------------

   overriding procedure Chosen_Interpretation
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Env);
      pragma Unreferenced (Set);
   begin
      Result := 0;
   end Chosen_Interpretation;

   -----------------
   -- Direct_Name --
   -----------------

   overriding procedure Direct_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Name);
   begin
      Result := 0;
   end Direct_Name;

end Gela.Plain_Interpretations;
