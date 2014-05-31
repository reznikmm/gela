------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Compilations;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Subunits;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Elements.Defining_Names;
with Gela.Interpretations;

package Gela.Pass_Utils is
   pragma Preelaborate;

   function Create_Unit_Declaration
     (Comp          : Gela.Compilations.Compilation_Access;
      Unit          : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Private_Index : Gela.Lexical_Types.Token_Count;
      Full_Name     : Gela.Lexical_Types.Symbol;
      Unit_Kind     : Gela.Semantic_Types.Unit_Kinds;
      With_List     : Gela.Lexical_Types.Symbol_List;
      Limited_With  : Gela.Lexical_Types.Symbol_List)
      return Gela.Semantic_Types.Env_Index;

   function Create_Unit_Body
     (Comp         : Gela.Compilations.Compilation_Access;
      Unit         : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access;
      Full_Name    : Gela.Lexical_Types.Symbol;
      Unit_Kind    : Gela.Semantic_Types.Unit_Kinds;
      With_List    : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List)
      return Gela.Semantic_Types.Env_Index;

   function Create_Subunit
     (Comp         : Gela.Compilations.Compilation_Access;
      Unit         : Gela.Elements.Subunits.Subunit_Access;
      Parent_Name  : Gela.Lexical_Types.Symbol;
      Full_Name    : Gela.Lexical_Types.Symbol;
      Unit_Kind    : Gela.Semantic_Types.Unit_Kinds;
      With_List    : Gela.Lexical_Types.Symbol_List;
      Limited_With : Gela.Lexical_Types.Symbol_List)
      return Gela.Semantic_Types.Env_Index;

   function Add_Name_Create_Region
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      Symbol       : Gela.Lexical_Types.Symbol;
      Name         : Gela.Elements.Defining_Names.Defining_Name_Access)
     return Gela.Semantic_Types.Env_Index;
   --  Add (Symbol, Name) to Env, then create new declarative region

   procedure Shall_Be_Subtype
     (Self   : Gela.Interpretations.Interpretation_Manager_Access;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index);
   --  Set of interpretation shall resolve to denote a subtype. 3.2.2 (8)

   procedure Resolve_To_Type
     (Self    : Gela.Interpretations.Interpretation_Manager_Access;
      Env     : Gela.Semantic_Types.Env_Index;
      Type_Up : Gela.Interpretations.Interpretation_Set_Index;
      Expr_Up : Gela.Interpretations.Interpretation_Set_Index;
      Result  : out Gela.Interpretations.Interpretation_Index);
   --  Resolve Type_Up to be type, then resolve Expr_Up have this type

end Gela.Pass_Utils;
