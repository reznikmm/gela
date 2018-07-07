--  This package provides Compilation_Manager interface and its methods.

with League.Strings;

with Gela.Contexts;
with Gela.Dependency_Lists;
with Gela.Lexical_Types;

package Gela.Compilation_Managers is
   pragma Preelaborate;

   type Compilation_Manager is limited interface;
   --  Compilation manager reads provided compilation or compilation unit
   --  together with any dependency of units.

   type Compilation_Manager_Access is access all Compilation_Manager'Class;
   for Compilation_Manager_Access'Storage_Size use 0;

   not overriding function Context
     (Self : Compilation_Manager) return Gela.Contexts.Context_Access
        is abstract;
   --  Return corresponding context

   not overriding procedure Read_Compilation
     (Self   : in out Compilation_Manager;
      Name   : League.Strings.Universal_String) is abstract;
   --  Read compilation with Name resolve and read all semantic dependency.
   --  Name interpreted by source finder object and usually just file name.

   not overriding procedure Read_Dependency
     (Self   : in out Compilation_Manager;
      List   : Gela.Dependency_Lists.Dependency_List_Access) is abstract;
   --  Resolve dependency provided by List and read needed compilation units.

   not overriding procedure Read_Declaration
     (Self   : in out Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol) is abstract;
   --  Read declaration with given symbol and any its dependecy.

   not overriding procedure Read_Body
     (Self   : in out Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol) is abstract;
   --  Read body with given symbol and any its dependecy.

end Gela.Compilation_Managers;
