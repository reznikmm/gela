--  This package provides Environment_Set for library level declarations.
with Gela.Contexts;
with Gela.Defining_Name_Cursors;
with Gela.Elements.Defining_Names;
with Gela.Environments;
with Gela.Lexical_Types;
with Gela.Semantic_Types;

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
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;

   overriding function Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : access Boolean)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class;

   Library_Env : constant Gela.Semantic_Types.Env_Index := 1;

private

   type Environment_Set (Context : access Gela.Contexts.Context'Class) is
     new Gela.Environments.Environment_Set with null record;

   overriding function Add_Defining_Name
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Enter_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   overriding function Leave_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index)
      return Gela.Semantic_Types.Env_Index;

end Gela.Library_Environments;