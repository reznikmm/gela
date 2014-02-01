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
     (Comp       : Gela.Compilations.Compilation_Access;
      Full_Name  : Gela.Lexical_Types.Symbol;
      Unit_Kind  : Gela.Semantic_Types.Unit_Kinds)
      return Gela.Semantic_Types.Env_Index
   is
      pragma Unreferenced (Comp);
      pragma Unreferenced (Full_Name);
      pragma Unreferenced (Unit_Kind);
   begin
      return 0;
   end Create_Unit;

end Gela.Pass_Utils;
