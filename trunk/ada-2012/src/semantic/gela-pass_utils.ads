------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Compilations;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Defining_Names;
with Gela.Elements.Defining_Identifiers;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Resolve;

package Gela.Pass_Utils is
   pragma Preelaborate;

   package Resolve renames Gela.Resolve;

   function Add_Name_Create_Region
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Decl   : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Env_Index;
   --  Add (Symbol, Name) to Env
   --  Check if Name is part of enumeration type declaration
   --  else create new declarative region

   function Leave_Declarative_Region
     (Comp   : Gela.Compilations.Compilation_Access;
      Index  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   function Add_Names
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      List         : Gela.Lexical_Types.Symbol_List;
      Names        : Gela.Elements.Defining_Identifiers
                       .Defining_Identifier_Sequence_Access)
      return Gela.Semantic_Types.Env_Index;
   --  Add (Symbol, Name) from List and Names to Env

   function Add_Names_Create_Region
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      List         : Gela.Lexical_Types.Symbol_List;
      Names        : Gela.Elements.Defining_Identifiers
                       .Defining_Identifier_Sequence_Access)
      return Gela.Semantic_Types.Env_Index;
   --  Add (Symbol, Name) from List and Names to Env, then create new
   --  declarative region

   function Parents_Declarative_Region
     (Comp          : Gela.Compilations.Compilation_Access;
      Full_Name     : Gela.Lexical_Types.Symbol)
      return Gela.Semantic_Types.Env_Index;
   --  Return end of declarative region of unit's parent

   function Create_String_Value
     (Comp          : Gela.Compilations.Compilation_Access;
      Full_Name     : Gela.Lexical_Types.Token_Index)
      return Gela.Semantic_Types.Value_Index;
   --  Return end of declarative region of unit's parent

   function Create_Function_Call_Value
     (Comp          : Gela.Compilations.Compilation_Access;
      Name          : Gela.Semantic_Types.Value_Index;
      Arguments     : Gela.Semantic_Types.Value_Index)
      return Gela.Semantic_Types.Value_Index;

   procedure End_Of_Compilation_Unit_Declaration
     (Comp   : Gela.Compilations.Compilation_Access;
      Unit   : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Symbol : Gela.Lexical_Types.Symbol;
      Env    : in out Gela.Semantic_Types.Env_Index);

end Gela.Pass_Utils;
