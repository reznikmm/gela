with Asis;

with Gela.Engines;
with Gela.Properties.Text;

package Gela.Rule.Empty is

   function Text
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
     return Gela.Properties.Text.Text;

end Gela.Rule.Empty;
