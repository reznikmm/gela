with Asis.Gela.Elements;
with Asis.Gela.Properties;

package body Asis.Gela.Replace is
   use Asis.Gela.Properties;

   ---------------------
   -- Set_Global_Kind --
   ---------------------

   procedure Set_Global_Kind
     (C       : Compilations.Compilation;
      Element : Element_Index;
      Value   : Global_Kinds)
   is
      use Asis.Gela.Compilations;
      use Compilations.Storage_Vectors;
   begin
      Set (C.Storage, Element, Global_Kinds'Pos (Value));
   end Set_Global_Kind;

   ----------------------------
   -- Access_To_Formal_Accee --
   ----------------------------

   procedure Access_To_Formal_Accee
     (C       : Compilations.Compilation;
      Element : Element_Index)
   is
      subtype Arg is Global_Kinds
        range An_Access_To_Variable .. An_Access_To_Protected_Function;

      Conv : constant array (Arg) of Global_Kinds :=
        (An_Access_To_Variable =>
           An_F_Access_To_Variable,

         An_Access_To_Constant =>
           An_F_Access_To_Constant,

         A_Pool_Specific_Access_To_Variable =>
           A_F_Pool_Specific_Access_To_Variable,

         An_Access_To_Procedure =>
           An_F_Access_To_Procedure,

         An_Access_To_Protected_Procedure =>
           An_F_Access_To_Protected_Procedure,

         An_Access_To_Function =>
           An_F_Access_To_Function,

         An_Access_To_Protected_Function =>
           An_F_Access_To_Protected_Function);

      Kind : constant Global_Kinds := Elements.Global_Kind (C, Element);
   begin
      Set_Global_Kind (C, Element, Conv (Kind));
   end Access_To_Formal_Accee;

end Asis.Gela.Replace;
