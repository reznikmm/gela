with Asis.Definitions;

package body Gela.Rule.Definitions.Simple_Expression_Range is

   ------------
   -- Length --
   ------------

   function Length
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Subtype_Indication;
      Property : Gela.Properties.Property_Name)
      return Gela.Properties.Text.Text
   is
      pragma Unreferenced (Property);

      Upper : constant Asis.Expression :=
        Asis.Definitions.Upper_Bound (Element);
   begin
      return Engine.Get (Upper, Gela.Properties.Value);
   end Length;

end Gela.Rule.Definitions.Simple_Expression_Range;
