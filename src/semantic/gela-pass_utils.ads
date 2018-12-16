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
with Gela.Elements.Number_Declarations;
with Gela.Elements.Use_Package_Clauses;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Resolve;
with Gela.Inheritance;
with Gela.Instantiation;
with Gela.Interpretations;

package Gela.Pass_Utils is
   pragma Preelaborate;

   package Resolve renames Gela.Resolve;
   package Instantiation renames Gela.Instantiation;
   package Inheritance renames Gela.Inheritance;

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

   function Create_Block_Region
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Block  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Env_Index;
   --  Add region for block_statements. If name /= null call
   --  Add_Name_Create_Region, otherwise just create new region

   function Leave_Declarative_Region
     (Comp   : Gela.Compilations.Compilation_Access;
      Index  : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Semantic_Types.Env_Index;

   function Create_Completion_Region
     (Comp   : Gela.Compilations.Compilation_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Decl   : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Env_Index;
   --  If corresponding declaration is not found, call Add_Name_Create_Region

   function Add_Names
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      List         : Gela.Lexical_Types.Symbol_List;
      Names        : Gela.Elements.Defining_Identifiers
                       .Defining_Identifier_Sequence_Access)
      return Gela.Semantic_Types.Env_Index;
   --  Add (Symbol, Name) from List and Names to Env

   function Add_Use_Package
     (Comp : Gela.Compilations.Compilation_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Node : not null Gela.Elements.Use_Package_Clauses.
        Use_Package_Clause_Access)
      return Gela.Semantic_Types.Env_Index;
   --  Add "use {Symbol};" to Env

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

   function Create_Numeric_Value
     (Comp  : Gela.Compilations.Compilation_Access;
      Value : Gela.Lexical_Types.Token_Index)
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
      Env    : Gela.Semantic_Types.Env_Index);

   procedure Add_Implicit_Declarations
     (Comp : Gela.Compilations.Compilation_Access;
      Tipe : Gela.Elements.Element_Access;
      Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Env  : in out Gela.Semantic_Types.Env_Index);

   procedure Choose_Auxiliary_Apply_Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Down   : Gela.Interpretations.Interpretation_Index;
      Result : in out Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds);
   --  Maybe move it into separate unit???

   procedure Choose_Composite_Constraint_Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Node   : access Gela.Elements.Element'Class;
      Result : out Gela.Interpretations.Constraint_Kinds);

   procedure Choose_Number_Declaration_Interpretation
     (Comp   : Gela.Compilations.Compilation_Access;
      Node   : access Gela.Elements.Number_Declarations.
        Number_Declaration'Class;
      Result : out Gela.Interpretations.Number_Declaration_Kinds);

end Gela.Pass_Utils;
