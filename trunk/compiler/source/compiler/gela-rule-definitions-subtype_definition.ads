with Asis;

with Gela.Engines;
with Gela.Properties.Text;

package Gela.Rule.Definitions.Subtype_Definition is

   function Length
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Subtype_Indication;
      Property : Gela.Properties.Property_Name)
     return Gela.Properties.Text.Text;

end Gela.Rule.Definitions.Subtype_Definition;
