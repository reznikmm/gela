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

   function Non_Static_Value
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      Def : constant Asis.Defining_Name :=
        Asis.Expressions.Corresponding_Name_Definition (Element);
   begin
      return Engine.Get (Def, Property);
   end Non_Static_Value;

end Gela.Rule.Expressions.Identifier;
