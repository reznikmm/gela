with Asis.Extensions.Static_Expressions;
with Gela.Rule.Join_Nested;

package body Gela.Rule.Expressions is

   ------------
   -- Global --
   ------------

   function Global
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      Val : constant Asis.Extensions.Static_Expressions.Value :=
        Asis.Extensions.Static_Expressions.Static_Value (Element);
   begin
      if not Asis.Extensions.Static_Expressions.Is_Static (Val) then
         return Gela.Rule.Join_Nested.Text (Engine, Element, Property);
      end if;

      declare
         Image : constant Wide_String := Val.Value_Image;
         Text  : String (Image'Range);

         Tipe   : Gela.Properties.Text.Text;  --  [N x i8]
         Result : Gela.Properties.Text.Text;
         Id    : String := Gela.Engines.Mapped_Element'Image
           (Engine.Map (Element));
      begin
         Id (1) := '.';
         for J in Image'Range loop
            Text (J) := Character'Val (Wide_Character'Pos (Image (J)));
         end loop;

         Tipe := Engine.Text_Container.Literal ("[");
         Tipe := Engine.Text_Container.Join (Tipe, Image'Length - 2);
         Tipe := Engine.Text_Container.Join (Tipe, " x i8] ");

         Result := Engine.Get (Element, Gela.Properties.Value);
         Result := Engine.Text_Container.Join (Result, " = ");
         Result := Engine.Text_Container.Join (Result, "unnamed_addr ");
         Result := Engine.Text_Container.Join (Result, "constant ");
         Result := Engine.Text_Container.Join (Result, "%_ada_string ");

         Result := Engine.Text_Container.Join
           (Result, "{ i8* getelementptr inbounds (");

         Result := Engine.Text_Container.Join (Result, Tipe);
         Result := Engine.Text_Container.Join (Result, "* ");
         Result := Engine.Text_Container.Join (Result, "@_str.data");

         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.Literal (Id));

         Result := Engine.Text_Container.Join
           (Result, ", i32 0, i32 0), i32 1, i32 ");

         Result := Engine.Text_Container.Join (Result, Image'Length - 2);
         Result := Engine.Text_Container.Join (Result, "}");

         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         Result := Engine.Text_Container.Join (Result, "@_str.data");
         Result := Engine.Text_Container.Join (Result, Id);
         Result := Engine.Text_Container.Join (Result, " = ");
         Result := Engine.Text_Container.Join (Result, "unnamed_addr ");
         Result := Engine.Text_Container.Join (Result, "constant ");
         Result := Engine.Text_Container.Join (Result, Tipe);
         Result := Engine.Text_Container.Join (Result, " c");
         Result := Engine.Text_Container.Join (Result, Text);

         Result := Engine.Text_Container.Join
           (Result, Engine.Text_Container.New_Line);

         return Result;
      end;
   end Global;

   -----------
   -- Value --
   -----------

   function Value
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      pragma Unreferenced (Property);
      Val : constant Asis.Extensions.Static_Expressions.Value :=
        Asis.Extensions.Static_Expressions.Static_Value (Element);
   begin
      if Asis.Extensions.Static_Expressions.Is_Static (Val) then
         declare
            Id     : constant Gela.Engines.Mapped_Element :=
              Engine.Map (Element);
            Image  : String := Gela.Engines.Mapped_Element'Image (Id);
            Result : Gela.Properties.Text.Text;
         begin
            Image (Image'First) := '.';

            Result := Engine.Text_Container.Literal ("@_str");

            Result := Engine.Text_Container.Join
              (Result, Engine.Text_Container.Literal (Image));

            return Result;
         end;
      else
         return Engine.Get (Element, Gela.Properties.Non_Static_Value);
      end if;
   end Value;

end Gela.Rule.Expressions;
