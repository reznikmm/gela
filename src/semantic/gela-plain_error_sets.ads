--  This package provides Environment_Set.
with Gela.Contexts;
with Gela.Error_Sets;
with Gela.Semantic_Types;

package Gela.Plain_Error_Sets is
   pragma Preelaborate;

   type Error_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Error_Sets.Error_Set with private;
   type Plain_Error_Set_Access is access all Error_Set;

private

   type Error_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Error_Sets.Error_Set with record
      Last : Gela.Semantic_Types.Error_Set_Index := 0;
   end record;

   overriding procedure Add
     (Self  : in out Error_Set;
      Prev  : Gela.Semantic_Types.Error_Set_Index;
      Next  : out Gela.Semantic_Types.Error_Set_Index);

   overriding procedure Join
     (Self   : in out Error_Set;
      Prev_1 : Gela.Semantic_Types.Error_Set_Index;
      Prev_2 : Gela.Semantic_Types.Error_Set_Index;
      Prev_3 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_4 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_5 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_6 : Gela.Semantic_Types.Error_Set_Index := 0;
      Next   : out Gela.Semantic_Types.Error_Set_Index);

end Gela.Plain_Error_Sets;
