with Asis.Declarations;

package body Gela.Rule.Declarations.Subprogram is

   ------------
   -- Global --
   ------------

   function Global
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      pragma Unreferenced (Property);
      use type Asis.List_Index;

      Result : Gela.Properties.Text.Text;
      Name   : constant Asis.Defining_Name :=
        Asis.Declarations.Names (Element) (1);
      List   : constant Asis.Parameter_Specification_List :=
        Asis.Declarations.Parameter_Profile (Element);
   begin
      Result := Engine.Text_Container.Literal ("declare void ");

      Result := Engine.Text_Container.Join
        (Result, Engine.Get (Name, Gela.Properties.Value));

      Result := Engine.Text_Container.Join (Result, "(");

      for J in List'Range loop
         Result := Engine.Text_Container.Join (Result, "%_ada_string* ");

         if J /= List'Last then
            Result := Engine.Text_Container.Join (Result, ", ");
         end if;
      end loop;

      Result := Engine.Text_Container.Join (Result, ")");

      Result := Engine.Text_Container.Join
        (Result, Engine.Text_Container.New_Line);

      return Result;
   end Global;

end Gela.Rule.Declarations.Subprogram;
