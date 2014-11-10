with Asis.Elements;
with Asis.Iterator;

package body Gela.Rule.Join_Nested is

   ----------
   -- Text --
   ----------

   function Text
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      procedure Pre_Operation
        (Item    : in     Asis.Element;
         Control : in out Asis.Traverse_Control;
         State   : in out Gela.Properties.Text.Text);

      procedure Post_Operation
        (Element : in     Asis.Element;
         Control : in out Asis.Traverse_Control;
         State   : in out Gela.Properties.Text.Text) is null;

      procedure Traverse is new
        Asis.Iterator.Traverse_Element
          (State_Information => Gela.Properties.Text.Text,
           Pre_Operation     => Pre_Operation,
           Post_Operation    => Post_Operation);

      -------------------
      -- Pre_Operation --
      -------------------

      procedure Pre_Operation
        (Item    : in     Asis.Element;
         Control : in out Asis.Traverse_Control;
         State   : in out Gela.Properties.Text.Text) is
      begin
         if not Asis.Elements.Is_Identical (Item, Element) then
            Control := Asis.Abandon_Children;

            State := Engine.Text_Container.Join
              (State, Engine.Get (Item, Property));
         end if;
      end Pre_Operation;

      Control : Asis.Traverse_Control := Asis.Continue;
      Result : Gela.Properties.Text.Text := Engine.Text_Container.Literal ("");
   begin
      Traverse (Element, Control, Result);

      return Result;
   end Text;

end Gela.Rule.Join_Nested;
