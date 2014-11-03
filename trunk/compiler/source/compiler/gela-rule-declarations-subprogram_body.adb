with Asis.Declarations;

package body Gela.Rule.Declarations.Subprogram_Body is

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
      List   : constant Asis.Statement_List :=
        Asis.Declarations.Body_Statements (Element);
   begin
      Result := Engine.Text_Container.Literal ("define void aaa () {");
      for J in List'Range loop
         Result := Engine.Text_Container.Join
           (Result, Engine.Get (List (J), Property));
      end loop;

      Result := Engine.Text_Container.Join (Result, "}");

      return Result;
   end Code;

end Gela.Rule.Declarations.Subprogram_Body;
