with Asis.Declarations;

package body Gela.Rule.Declarations.Variable is

   ----------
   -- Code --
   ----------

   function Code
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      procedure Store
        (Result  : in out Gela.Properties.Text.Text;
         Index   : Natural;
         Item_Tp : String;
         Value   : Gela.Properties.Text.Text);

      procedure Store
        (Result  : in out Gela.Properties.Text.Text;
         Index   : Natural;
         Item_Tp : String;
         Value   : Gela.Properties.Text.Text)
      is
         Tmp : constant Positive := Engine.Unique;
      begin
         Result := Engine.Text_Container.Join (Result, " %");
         Result := Engine.Text_Container.Join (Result, Tmp);
         Result := Engine.Text_Container.Join
           (Result,
            " = getelementptr inbounds %_ada_string * %MAX_LEN_STRING_LIT," &
            " i32 0, i32 ");
         Result := Engine.Text_Container.Join (Result, Index);
         Result := Engine.Text_Container.Join_New_Line (Result);
         Result := Engine.Text_Container.Join (Result, " store ");
         Result := Engine.Text_Container.Join (Result, Item_Tp);
         Result := Engine.Text_Container.Join (Result, " ");
         Result := Engine.Text_Container.Join (Result, Value);
         Result := Engine.Text_Container.Join (Result, ", ");
         Result := Engine.Text_Container.Join (Result, Item_Tp);
         Result := Engine.Text_Container.Join (Result, "* %");
         Result := Engine.Text_Container.Join (Result, Tmp);
         Result := Engine.Text_Container.Join_New_Line (Result);
      end Store;


      Name   : Gela.Properties.Text.Text;
      Result : Gela.Properties.Text.Text;
      Subtp  : constant Asis.Definition :=
        Asis.Declarations.Object_Declaration_Subtype (Element);
      Names  : constant Asis.Defining_Name_List :=
        Asis.Declarations.Names (Element);
   begin
      Result := Engine.Get (Subtp, Property);

      for J in Names'Range loop
         Name := Engine.Get (Names (J), Gela.Properties.Non_Static_Value);

         Result := Engine.Text_Container.Join (Result, Name);
         Result := Engine.Text_Container.Join
           (Result, ".address = alloca i8, i32 ");
         Result := Engine.Text_Container.Join
           (Result, Engine.Get (Subtp, Gela.Properties.Length));
         Result := Engine.Text_Container.Join_New_Line (Result);

         Result := Engine.Text_Container.Join (Result, Name);
         Result := Engine.Text_Container.Join
           (Result, " = alloca %_ada_string");
         Result := Engine.Text_Container.Join_New_Line (Result);

         Name := Engine.Text_Container.Join (Name, ".address");

         Store (Result, 0, "i8*", Name);
         Store (Result, 1, "i32", Engine.Text_Container.Literal ("1"));
         Store (Result, 2, "i32", Engine.Get (Subtp, Gela.Properties.Length));
      end loop;

      Result := Engine.Text_Container.Join_New_Line (Result);

      return Result;
   end Code;

   --------------
   -- Is_Local --
   --------------

   function Is_Local
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Boolean_Property_Name)
      return Boolean
   is
      pragma Unreferenced (Engine, Element, Property);
   begin
      return True;
   end Is_Local;

end Gela.Rule.Declarations.Variable;
