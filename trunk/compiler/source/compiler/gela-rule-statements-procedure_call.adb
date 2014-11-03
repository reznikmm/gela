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
      Result : Gela.Properties.Text.Text;
      Prefix : constant Asis.Expression :=
        Asis.Statements.Called_Name (Element);
   begin
      Result := Engine.Text_Container.Literal (" call void;");

      Result := Engine.Text_Container.Join
        (Result, Engine.Get (Prefix, Property));

      return Result;
   end Code;

end Gela.Rule.Statements.Procedure_Call;
