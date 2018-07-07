package body Gela.Plain_Error_Sets is

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Self  : in out Error_Set;
      Prev  : Gela.Semantic_Types.Error_Set_Index;
      Next  : out Gela.Semantic_Types.Error_Set_Index)
   is
      pragma Unreferenced (Prev);
      use type Gela.Semantic_Types.Error_Set_Index;
   begin
      Self.Last := Self.Last + 1;
      Next := Self.Last;
   end Add;

   ----------
   -- Join --
   ----------

   overriding procedure Join
     (Self   : in out Error_Set;
      Prev_1 : Gela.Semantic_Types.Error_Set_Index;
      Prev_2 : Gela.Semantic_Types.Error_Set_Index;
      Prev_3 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_4 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_5 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_6 : Gela.Semantic_Types.Error_Set_Index := 0;
      Next   : out Gela.Semantic_Types.Error_Set_Index)
   is
      pragma Unreferenced (Prev_1);
      pragma Unreferenced (Prev_2);
      pragma Unreferenced (Prev_3);
      pragma Unreferenced (Prev_4);
      pragma Unreferenced (Prev_5);
      pragma Unreferenced (Prev_6);
      use type Gela.Semantic_Types.Error_Set_Index;
   begin
      Self.Last := Self.Last + 1;
      Next := Self.Last;
   end Join;

end Gela.Plain_Error_Sets;
