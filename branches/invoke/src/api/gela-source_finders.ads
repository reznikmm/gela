with League.Strings;

with Gela.Lexical_Types;

package Gela.Source_Finders is
   pragma Preelaborate;

   subtype File_Base_Name is League.Strings.Universal_String;

   type Source_Finder is limited interface;
   type Source_Finder_Access is access all Source_Finder'Class;
   for Source_Finder_Access'Storage_Size use 0;

   not overriding procedure Lookup_Compilation
     (Self   : Source_Finder;
      Name   : League.Strings.Universal_String;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String) is abstract;

   not overriding procedure Lookup_Declaration
     (Self   : Source_Finder;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String) is abstract;

   not overriding procedure Lookup_Body
     (Self   : Source_Finder;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String) is abstract;

end Gela.Source_Finders;
