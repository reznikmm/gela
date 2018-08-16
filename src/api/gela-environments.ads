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

   not overriding function Library_Level_Environment
     (Self  : Environment_Set)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Return environment that incudes only library level names.

   not overriding function Add_With_Clause
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by adding with clause for given Symbol
   --  to provided env with given Index. Return index of created environment

   not overriding function Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Return environment index corresponding to unit of given Symbol
   --  This environment points to region of corresponding declaration and
   --  includes all child units at the end of region.

   not overriding procedure Set_Library_Unit_Environment
     (Self   : access Environment_Set;
      Symbol : Gela.Lexical_Types.Symbol;
      Value  : Gela.Semantic_Types.Env_Index) is abstract;
   --  Save environment index as corresponding to unit of given Symbol
   --  After Value set as library unit environment for given Symbol
   --  any call to Add_Defining_Name will update mapping for the Symbol.

   not overriding function Direct_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class is abstract;
   --  Return list of direct visible defining names from the environment
   --  pointed by Index with given Symbol.

   not overriding function Use_Visible
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class is abstract;
   --  Return list of use visible defining names from the environment
   --  pointed by Index with given Symbol.

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
   --  If Region is null, use current (top) region to search in.

   not overriding function Add_Defining_Name
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by adding (Symbol, Name) to provided env with
   --  given Index. Return index of created environment

   not overriding function Add_Use_Package
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by adding use package <Name> to provided env
   --  with given Index. Return index of created environment

   not overriding function Add_Rename_Package
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by adding package <Region> renames <Name> to env
   --  with given Index. Return index of created environment

   not overriding function Add_Completion
     (Self       : in out Environment_Set;
      Index      : Gela.Semantic_Types.Env_Index;
      Name       : Gela.Elements.Defining_Names.Defining_Name_Access;
      Completion : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by adding (Name -> Completion) mapping to
   --  provided env with given Index. Return index of created environment

   subtype Completion_Index is Natural range 0 .. 6;

   type Completion_Array is array (Completion_Index range <>) of
     Gela.Elements.Defining_Names.Defining_Name_Access;

   type Completion_List (Length : Completion_Index := 0) is record
      Data : Completion_Array (1 .. Length);
   end record;

   not overriding function Completions
     (Self       : in out Environment_Set;
      Index      : Gela.Semantic_Types.Env_Index;
      Name       : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Completion_List is abstract;
   --  Find completions for given Name in provided env with given Index.

   not overriding function Enter_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by extending provided env with new empty
   --  declarative region named by Region defining name.
   --  Return index of created environment

   not overriding function Enter_Completion_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Region : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by extending provided env with completion of
   --  declarative region named by Region defining name.
   --  Return index of created environment

   not overriding function Leave_Declarative_Region
     (Self   : access Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index)
      return Gela.Semantic_Types.Env_Index is abstract;
   --  Create new environment by closing top declarative region in provided env
   --  Return index of created environment

end Gela.Environments;
