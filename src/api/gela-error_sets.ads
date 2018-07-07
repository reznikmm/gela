--  This package provides Error_Set interface and its methods.
with Gela.Semantic_Types;

package Gela.Error_Sets is
   pragma Preelaborate;

   type Error_Set is limited interface;
   --  Set of all symbols encountered in compilation units of some context
   type Error_Set_Access is access all Error_Set'Class;
   for Error_Set_Access'Storage_Size use 0;

   not overriding procedure Add
     (Self  : in out Error_Set;
      Prev  : Gela.Semantic_Types.Error_Set_Index;
      Next  : out Gela.Semantic_Types.Error_Set_Index) is abstract;
   --  Create new error set by adding error to Prev error set

   not overriding procedure Join
     (Self   : in out Error_Set;
      Prev_1 : Gela.Semantic_Types.Error_Set_Index;
      Prev_2 : Gela.Semantic_Types.Error_Set_Index;
      Prev_3 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_4 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_5 : Gela.Semantic_Types.Error_Set_Index := 0;
      Prev_6 : Gela.Semantic_Types.Error_Set_Index := 0;
      Next   : out Gela.Semantic_Types.Error_Set_Index) is abstract;
   --  Create new error set by joining Prev_X error sets

end Gela.Error_Sets;
