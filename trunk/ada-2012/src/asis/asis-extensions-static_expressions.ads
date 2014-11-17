with League.Strings;

package Asis.Extensions.Static_Expressions is

   type Value is tagged private;

   function Static_Value (Expression : Asis.Expression) return Value;

   function Is_Static (Self : Value) return Boolean;
   function Value_Image (Self : Value) return Asis.Program_Text;

private

   type Value is tagged record
      Is_Static : Boolean := False;
      Image     : League.Strings.Universal_String;
   end record;

end Asis.Extensions.Static_Expressions;
