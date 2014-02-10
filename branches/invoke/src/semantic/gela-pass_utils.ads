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

end Gela.Pass_Utils;
