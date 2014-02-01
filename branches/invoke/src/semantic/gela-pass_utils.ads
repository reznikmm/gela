------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Compilations;
with Gela.Lexical_Types;
with Gela.Semantic_Types;

package Gela.Pass_Utils is
   pragma Preelaborate;

   function Create_Unit
     (Comp       : Gela.Compilations.Compilation_Access;
      Full_Name  : Gela.Lexical_Types.Symbol;
      Unit_Kind  : Gela.Semantic_Types.Unit_Kinds)
      return Gela.Semantic_Types.Env_Index;

end Gela.Pass_Utils;
