--  This package provides Environment_Set interface and its methods.
with Gela.Defining_Name_Cursors;
with Gela.Elements.Defining_Names;
with Gela.Lexical_Types;
with Gela.Semantic_Types;

package Gela.Environments is
   pragma Preelaborate;

   type Environment_Set is limited interface;
   type Environment_Set_Access is access all Environment_Set'Class;
   for Environment_Set_Access'Storage_Size use 0;

   not overriding procedure Library_Level_Environment
     (Self  : in out Environment_Set;
      Value : out Gela.Semantic_Types.Env_Index) is abstract;
   --  Return environment that incudes library level names.

   not overriding function Direct_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class is abstract;
   --  Return list of direct visible defining names from the environment
   --  pointed by Index with given Symbol.

   not overriding function Add_Direct_Visible
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by adding (Symbol, Name) to provided env with
   --  given Index. Return index of created set

   not overriding function Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Found  : access Boolean)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class is abstract;
   --  Return list of defining names from the environment pointed by Index
   --  with given Symbol which visible inside declarative region corresponding
   --  to given Region name. Return Found = False if no such region found.

end Gela.Environments;
