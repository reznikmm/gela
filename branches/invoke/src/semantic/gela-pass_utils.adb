------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Pass_Utils is

   -----------------
   -- Create_Unit --
   -----------------

   function Create_Unit
     (Comp : Gela.Compilations.Compilation_Access;
      Unit : Gela.Elements.Compilation_Units.Compilation_Unit_Access)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Comp);
      pragma Unreferenced (Unit);
   begin
      return 0;
   end Create_Unit;

end Gela.Pass_Utils;
