with Gela.Compilations;
with Gela.Element_Visiters;
with Gela.Elements.Function_Calls;
with Gela.Elements.String_Literals;
with Gela.Semantic_Types;

package body Asis.Extensions.Static_Expressions is

   ---------------
   -- Is_Static --
   ---------------

   function Is_Static (Self : Value) return Boolean is
   begin
      return Self.Is_Static;
   end Is_Static;

   ------------------
   -- Static_Value --
   ------------------

   function Static_Value (Expression : Asis.Expression) return Value is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Value_Index := 0;
         end record;

         overriding procedure Function_Call
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Calls.Function_Call_Access);

         overriding procedure String_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.String_Literals.
              String_Literal_Access);

      end Get;

      package body Get is

         overriding procedure Function_Call
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Calls.Function_Call_Access)
         is
         begin
            Self.Result := Node.Static_Value;
         end Function_Call;

         overriding procedure String_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.String_Literals.
              String_Literal_Access) is
         begin
            Self.Result := Node.Static_Value;
         end String_Literal;

      end Get;

      use type Gela.Semantic_Types.Value_Index;
      V : Get.Visiter;
   begin
      Check_Nil_Element (Expression, "Static_Value");
      Expression.Data.Visit (V);

      if V.Result = 0 then
         return (others => <>);
      else
         declare
            Comp  : constant Gela.Compilations.Compilation_Access :=
              Expression.Data.Enclosing_Compilation;
         begin
            return (True, Comp.Context.Values.Image (V.Result));
         end;
      end if;
   end Static_Value;

   -----------------
   -- Value_Image --
   -----------------

   function Value_Image (Self : Value) return Asis.Program_Text is
   begin
      return Self.Image.To_UTF_16_Wide_String;
   end Value_Image;

end Asis.Extensions.Static_Expressions;
