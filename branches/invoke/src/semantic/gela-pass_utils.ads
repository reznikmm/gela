------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Compilations;
with Gela.Elements.Compilation_Units;
with Gela.Semantic_Types;

package Gela.Pass_Utils is
   pragma Preelaborate;

   function Create_Unit
     (Comp : Gela.Compilations.Compilation_Access;
      Unit : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
      return Gela.Semantic_Types.Env_Index;

end Gela.Pass_Utils;
