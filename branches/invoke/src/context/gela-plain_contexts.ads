with Gela.Symbol_Sets;
with Gela.Source_Finders;
with Gela.Compilation_Unit_Sets;
with Gela.Lexers;
with Gela.Naming_Schemas;
with Gela.Unit_Containers;

with Gela.Contexts;
with Gela.Plain_Lexers;

package Gela.Plain_Contexts is
   pragma Preelaborate;

   type Context is new Gela.Contexts.Context with private;

   overriding function Symbols
     (Self : access Context) return Gela.Symbol_Sets.Symbol_Set_Access;

   overriding function Library_Unit_Declarations
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Compilation_Unit_Bodies
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;

   overriding function Unit_Containers
     (Self  : access Context) return Gela.Unit_Containers.Unit_Container_List;

   overriding function Naming_Schema
     (Self  : access Context) return Gela.Naming_Schemas.Naming_Schema_Access;

   overriding function Source_Finder
     (Self  : access Context) return Gela.Source_Finders.Source_Finder_Access;

   overriding function Lexer
     (Self  : access Context) return Gela.Lexers.Lexer_Access;

private
   type Context is new Gela.Contexts.Context with record
      Lexer : aliased Gela.Plain_Lexers.Lexer;
   end record;

end Gela.Plain_Contexts;
