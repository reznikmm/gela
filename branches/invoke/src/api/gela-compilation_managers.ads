with League.Strings;

with Gela.Contexts;
with Gela.Lexical_Types;
with Gela.Dependency_Lists;

package Gela.Compilation_Managers is
   pragma Preelaborate;

   type Compilation_Manager is limited interface;
   type Compilation_Manager_Access is access all Compilation_Manager'Class;
   for Compilation_Manager_Access'Storage_Size use 0;

   not overriding function Context
     (Self : Compilation_Manager) return Gela.Contexts.Context_Access
        is abstract;

   not overriding procedure Read_Compilation
     (Self   : Compilation_Manager;
      Name   : League.Strings.Universal_String) is abstract;

   not overriding procedure Read_Dependency
     (Self   : Compilation_Manager;
      List   : Gela.Dependency_Lists.Dependency_List_Access) is abstract;

   not overriding procedure Read_Declaration
     (Self   : Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol) is abstract;

   not overriding procedure Read_Body
     (Self   : Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol) is abstract;

end Gela.Compilation_Managers;
