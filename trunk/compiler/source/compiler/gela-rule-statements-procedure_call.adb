with Asis.Expressions;
with Asis.Statements;

package body Gela.Rule.Statements.Procedure_Call is

   ----------
   -- Code --
   ----------

   function Code
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      pragma Unreferenced (Property);
      use type Asis.List_Index;

      Result : Gela.Properties.Text.Text;
      Prefix : constant Asis.Expression :=
        Asis.Statements.Called_Name (Element);
      Arg    : Asis.Expression;
      Args   : constant Asis.Association_List :=
        Asis.Statements.Call_Statement_Parameters (Element);
   begin
      Result := Engine.Text_Container.Literal (" call void ");

      Result := Engine.Text_Container.Join
        (Result, Engine.Get (Prefix, Gela.Properties.Value));

      Result := Engine.Text_Container.Join (Result, "(");

      for J in Args'Range loop
         Arg := Asis.Expressions.Actual_Parameter (Args (J));

         Result := Engine.Text_Container.Join (Result, "%_ada_string* ");

         Result := Engine.Text_Container.Join
           (Result, Engine.Get (Arg, Gela.Properties.Value));

         if J /= Args'Last then
            Result := Engine.Text_Container.Join (Result, ", ");
         end if;
      end loop;

      Result := Engine.Text_Container.Join (Result, ")");

      Result := Engine.Text_Container.Join_New_Line (Result);

      return Result;
   end Code;

end Gela.Rule.Statements.Procedure_Call;
