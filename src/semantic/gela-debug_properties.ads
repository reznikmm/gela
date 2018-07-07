with League.Strings;

with Gela.Elements;

package Gela.Debug_Properties is
   pragma Preelaborate;

   procedure Dump
     (Element : Gela.Elements.Element_Access;
      Debug   : League.Strings.Universal_String);

end Gela.Debug_Properties;
