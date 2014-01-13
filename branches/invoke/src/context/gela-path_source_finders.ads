with League.Strings;

with Gela.Lexical_Types;
with Gela.Source_Finders;
with Gela.Contexts;

package Gela.Path_Source_Finders is
   pragma Preelaborate;

   subtype File_Base_Name is League.Strings.Universal_String;

   type Source_Finder (Context : access Gela.Contexts.Context'Class) is
     limited new Gela.Source_Finders.Source_Finder with private;

   function Create
     (Path    : League.Strings.Universal_String;
      Context : access Gela.Contexts.Context'Class;
      Next    : Gela.Source_Finders.Source_Finder_Access := null)
      return Gela.Source_Finders.Source_Finder_Access;

private

   type Source_Finder (Context : access Gela.Contexts.Context'Class) is
     limited new Gela.Source_Finders.Source_Finder with
   record
      Directory : League.Strings.Universal_String;
      Next      : Gela.Source_Finders.Source_Finder_Access;
   end record;

   overriding procedure Lookup_Compilation
     (Self   : Source_Finder;
      Name   : League.Strings.Universal_String;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String);

   overriding procedure Lookup_Declaration
     (Self   : Source_Finder;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String);

   overriding procedure Lookup_Body
     (Self   : Source_Finder;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : out Boolean;
      File   : out League.Strings.Universal_String;
      Source : out League.Strings.Universal_String);

end Gela.Path_Source_Finders;
