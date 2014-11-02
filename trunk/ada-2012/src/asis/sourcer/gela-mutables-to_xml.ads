------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with Gela.Grammars;

package Gela.Mutables.To_XML is

   function Compilation
     (C   : Gela.Mutables.Mutable_Compilation_Access;
      AST : Gela.Grammars.Grammar)
      return League.Strings.Universal_String;

end Gela.Mutables.To_XML;
