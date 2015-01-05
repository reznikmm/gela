with Asis;

with Gela.Engines;
with Gela.Properties.Text;

package Gela.Rule.Declarations.Variable is

   function Code
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Property_Name)
     return Gela.Properties.Text.Text;

   function Is_Local
     (Engine   : access Gela.Engines.Engine;
      Element  : Asis.Element;
      Property : Gela.Properties.Boolean_Property_Name)
     return Boolean;

end Gela.Rule.Declarations.Variable;
