with League.Strings;

with Gela.Contexts;
with Gela.Compilation_Managers;
with Gela.Dependency_Lists;
with Gela.Lexical_Types;

package Gela.Plain_Compilation_Managers is
   pragma Preelaborate;

   type Compilation_Manager (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Compilation_Managers.Compilation_Manager with private;

   type Compilation_Manager_Access is access all Compilation_Manager;

private
   type Compilation_Manager (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Compilation_Managers.Compilation_Manager with
   record
      null;
   end record;

   overriding function Context
     (Self : Compilation_Manager) return Gela.Contexts.Context_Access;

   overriding procedure Read_Compilation
     (Self   : Compilation_Manager;
      Name   : League.Strings.Universal_String);

   overriding procedure Read_Dependency
     (Self   : Compilation_Manager;
      List   : Gela.Dependency_Lists.Dependency_List_Access);

   overriding procedure Read_Declaration
     (Self   : Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol);

   overriding procedure Read_Body
     (Self   : Compilation_Manager;
      Symbol : Gela.Lexical_Types.Symbol);

end Gela.Plain_Compilation_Managers;
