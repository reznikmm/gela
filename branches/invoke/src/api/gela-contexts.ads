with Gela.Symbol_Sets;
lImited with Gela.Source_Finders;
limited with Gela.Compilation_Unit_Sets;
limited with Gela.Lexers;
limited with Gela.Naming_Schemas;
limited with Gela.Unit_Containers;

package Gela.Contexts is
   pragma Preelaborate;

   type Context is limited interface;
   type Context_Access is access all Context'Class;
   for Context_Access'Storage_Size use 0;

   not overriding function Symbols
     (Self : access Context)
      return Gela.Symbol_Sets.Symbol_Set_Access is abstract;

   not overriding function Library_Unit_Declarations
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;

   not overriding function Compilation_Unit_Bodies
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;

   not overriding function Unit_Containers
     (Self  : access Context)
      return Gela.Unit_Containers.Unit_Container_List is abstract;

   not overriding function Naming_Schema
     (Self  : access Context)
      return Gela.Naming_Schemas.Naming_Schema_Access is abstract;

   not overriding function Source_Finder
     (Self  : access Context)
      return Gela.Source_Finders.Source_Finder_Access is abstract;

   not overriding function Lexer
     (Self  : access Context)
      return Gela.Lexers.Lexer_Access is abstract;

end Gela.Contexts;
