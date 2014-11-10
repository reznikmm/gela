package body Gela.Rule.Empty is

   ----------
   -- Text --
   ----------

   function Text
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      pragma Unreferenced (Element, Property);
   begin
      return Engine.Text_Container.Literal ("");
   end Text;

end Gela.Rule.Empty;
