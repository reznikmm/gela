--  This package provides Environment_Set for library level declarations.
with Gela.Contexts;
with Gela.Defining_Name_Cursors;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Environments;

package Gela.Library_Environments is
   pragma Preelaborate;

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with private;
   type Environment_Set_Access is access all Environment_Set'Class;
   for Environment_Set_Access'Storage_Size use 0;

   overriding procedure Library_Level_Environment
     (Self  : in out Environment_Set;
      Value : out Gela.Semantic_Types.Env_Index);
   --  Return environment that incudes library level names.

   overriding function Direct_Visible
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;
   --  Return list of defining names from the environment pointed by Index
   --  with given Symbol.

private

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with null record;

end Gela.Library_Environments;
