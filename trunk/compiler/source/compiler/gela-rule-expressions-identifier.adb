with Asis.Elements;
with Asis.Expressions;

package body Gela.Rule.Expressions.Identifier is

   ------------
   -- Global --
   ------------

   function Global
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      Def    : constant Asis.Defining_Name :=
        Asis.Expressions.Corresponding_Name_Definition (Element);
   begin
      if Asis.Elements.Is_Nil (Def) then
         return Engine.Text_Container.Literal ("");
      else
         return Engine.Get (Asis.Elements.Enclosing_Element (Def), Property);
      end if;
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
      Image  : constant Wide_String := Asis.Expressions.Name_Image (Element);
      Txt    : String (Image'Range);
      Result : Gela.Properties.Text.Text;
   begin
      for J in Image'Range loop
         Txt (J) := Character'Val (Wide_Character'Pos (Image (J)));
      end loop;

      Result := Engine.Text_Container.Literal ("@");
      Result := Engine.Text_Container.Join (Result, Txt);

      return Result;
   end Value;

end Gela.Rule.Expressions.Identifier;
