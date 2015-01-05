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
      use type Asis.Element_List;

      Result : Gela.Properties.Text.Text;
      Name   : constant Asis.Defining_Name :=
        Asis.Declarations.Names (Element) (1);
      List   : constant Asis.Statement_List :=
        Asis.Declarations.Body_Declarative_Items (Element) &
        Asis.Declarations.Body_Statements (Element);
   begin
      Result := Engine.Text_Container.Literal
        ("%_ada_string = type { i8*, i32, i32 }");
      Result := Engine.Text_Container.Join_New_Line (Result);

      Result := Engine.Text_Container.Join
        (Result, "declare void @llvm.trap() noreturn nounwind");

      Result := Engine.Text_Container.Join_New_Line (Result);

      Result := Engine.Text_Container.Join
        (Result,
         "declare void @llvm.memmove.p0i8.p0i8.i32(i8*, i8*, i32, i32, i1)");

      Result := Engine.Text_Container.Join_New_Line (Result);

      Result := Engine.Text_Container.Join
        (Result, Engine.Get (Element, Gela.Properties.Global));

      Result := Engine.Text_Container.Join (Result, "define void ");

      Result := Engine.Text_Container.Join
        (Result, Engine.Get (Name, Gela.Properties.Non_Static_Value));

      Result := Engine.Text_Container.Join (Result, " () {");

      Result := Engine.Text_Container.Join_New_Line (Result);

      for J in List'Range loop
         Result := Engine.Text_Container.Join
           (Result, Engine.Get (List (J), Property));
      end loop;

      Result := Engine.Text_Container.Join (Result, " ret void");

      Result := Engine.Text_Container.Join_New_Line (Result);

      Result := Engine.Text_Container.Join (Result, "}");

      return Result;
   end Code;

end Gela.Rule.Declarations.Subprogram_Body;
