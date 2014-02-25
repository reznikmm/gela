--  This package provides Context interface and its methods.

with Gela.Environments;
with Gela.Interpretations;
with Gela.Symbol_Sets;
limited with Gela.Compilation_Managers;
limited with Gela.Compilation_Unit_Sets;
limited with Gela.Dependency_Lists;
limited with Gela.Lexers;
limited with Gela.Naming_Schemas;
limited with Gela.Source_Finders;
limited with Gela.Unit_Containers;

package Gela.Contexts is
   pragma Preelaborate;

   type Context is limited interface;
   --  Context provides view to an Ada environment by queries for
   --  unit declarations, bodies and containers.
   --  Beside this Context provides set of auxilary objects to help to
   --  find sources, make lexic analisys, resolve dependencies, populate
   --  context with new units, etc.

   type Context_Access is access all Context'Class;
   for Context_Access'Storage_Size use 0;

   not overriding function Library_Unit_Declarations
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;
   --  Return set of library unit declarations of given context

   not overriding function Compilation_Unit_Bodies
     (Self  : access Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
        is abstract;
   --  Return set of library unit bodies and subunits of given context

   not overriding function Unit_Containers
     (Self  : access Context)
      return Gela.Unit_Containers.Unit_Container_List is abstract;
   --  Return list of unit containers of given context

   not overriding function Symbols
     (Self : access Context)
      return Gela.Symbol_Sets.Symbol_Set_Access is abstract;
   --  Return set of symbols used in all units of given context

   not overriding function Naming_Schema
     (Self  : access Context)
      return Gela.Naming_Schemas.Naming_Schema_Access is abstract;
   --  Return naming schema

   not overriding function Source_Finder
     (Self  : access Context)
      return Gela.Source_Finders.Source_Finder_Access is abstract;
   --  Return source finder/loader

   not overriding function Lexer
     (Self  : access Context)
      return Gela.Lexers.Lexer_Access is abstract;
   --  Return Ada lexic analyzer

   not overriding function Compilation_Manager
     (Self  : access Context)
      return Gela.Compilation_Managers.Compilation_Manager_Access is abstract;
   --  Return compilation manager capable to read units into given context

   not overriding function Dependency_List
     (Self  : access Context)
      return Gela.Dependency_Lists.Dependency_List_Access is abstract;
   --  Return object to trace semantic dependency between units of the context

   not overriding function Environment_Set
     (Self  : access Context)
      return Gela.Environments.Environment_Set_Access is abstract;
   --  Return Environment set

   not overriding function Interpretation_Manager
     (Self  : access Context)
      return Gela.Interpretations.Interpretation_Manager_Access is abstract;
   --  Return interpretation manager

end Gela.Contexts;
