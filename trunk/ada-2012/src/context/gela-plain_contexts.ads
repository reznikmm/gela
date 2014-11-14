with League.Strings;

with Gela.Compilation_Managers;
with Gela.Compilation_Unit_Factories;
with Gela.Compilation_Unit_Sets;
with Gela.Compilation_Units;
with Gela.Contexts;
with Gela.Dependency_Lists;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Subunits;
with Gela.Environments;
with Gela.Error_Sets;
with Gela.GNAT_Naming_Schemas;
with Gela.Interpretations;
with Gela.Lexers;
with Gela.Lexical_Types;
with Gela.Naming_Schemas;
with Gela.Plain_Environments;
with Gela.Plain_Error_Sets;
with Gela.Plain_Interpretations;
with Gela.Plain_Lexers;
with Gela.Plain_Symbol_Sets;
with Gela.Plain_Type_Managers;
with Gela.Source_Finders;
with Gela.Symbol_Sets;
with Gela.Type_Managers;
with Gela.Unit_Containers;
with Gela.Value_Sets;

package Gela.Plain_Contexts is
   pragma Preelaborate;

   type Context is limited new Gela.Contexts.Context
     and Gela.Compilation_Unit_Factories.Compilation_Unit_Factory with private;

   not overriding procedure Initialize
     (Self : in out Context;
      Env  : League.Strings.Universal_String;
      Path : League.Strings.Universal_String;
      Comp : League.Strings.Universal_String);

private

   type Context is limited new Gela.Contexts.Context
     and Gela.Compilation_Unit_Factories.Compilation_Unit_Factory
   with record
      Specs   : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
      Bodies  : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
      Symbols : aliased Gela.Plain_Symbol_Sets.Symbol_Set;
      Lexer   : aliased Gela.Plain_Lexers.Lexer (Context'Unchecked_Access);
      Finder  : Gela.Source_Finders.Source_Finder_Access;
      Manager : Gela.Compilation_Managers.Compilation_Manager_Access;
      Values  : Gela.Value_Sets.Value_Set_Access;
      Schema  : aliased Gela.GNAT_Naming_Schemas.Naming_Schema
        (Context'Unchecked_Access);
      Env     : aliased Gela.Plain_Environments.Environment_Set
        (Context'Unchecked_Access);
      Dependency_List : Gela.Dependency_Lists.Dependency_List_Access;
      Interpretation : aliased
        Gela.Plain_Interpretations.Interpretation_Manager
          (Context'Unchecked_Access);
      Errors : aliased Gela.Plain_Error_Sets.Error_Set
        (Context'Unchecked_Access);
      Types  : aliased Gela.Plain_Type_Managers.Type_Manager
        (Context'Unchecked_Access);
   end record;

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

   overriding function Compilation_Manager
     (Self  : access Context)
      return Gela.Compilation_Managers.Compilation_Manager_Access;

   overriding function Dependency_List
     (Self  : access Context)
      return Gela.Dependency_Lists.Dependency_List_Access;

   overriding function Environment_Set
     (Self  : access Context)
      return Gela.Environments.Environment_Set_Access;

   overriding function Interpretation_Manager
     (Self  : access Context)
      return Gela.Interpretations.Interpretation_Manager_Access;

   overriding function Create_Library_Unit_Declaration
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Package_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access;

   overriding function Create_Body_Unit_Without_Declaration
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Package_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
      return Gela.Compilation_Units.Body_Unit_Access;

   overriding function Create_Body_Unit
     (Self        : in out Context;
      Declaration : Gela.Compilation_Units.Library_Unit_Declaration_Access;
      Name        : Gela.Lexical_Types.Symbol;
      Node        : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
      return Gela.Compilation_Units.Body_Unit_Access;

   overriding function Create_Subunit
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Body_Unit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Subunits.Subunit_Access)
      return Gela.Compilation_Units.Subunit_Access;

   overriding function Create_Subunit
     (Self   : in out Context;
      Parent : Gela.Compilation_Units.Subunit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Node   : Gela.Elements.Subunits.Subunit_Access)
      return Gela.Compilation_Units.Subunit_Access;

   overriding function Error_Set
     (Self  : access Context) return Gela.Error_Sets.Error_Set_Access;

   overriding function Types
     (Self  : access Context) return Gela.Type_Managers.Type_Manager_Access;

   overriding function Values
     (Self : access Context) return Gela.Value_Sets.Value_Set_Access;

end Gela.Plain_Contexts;
