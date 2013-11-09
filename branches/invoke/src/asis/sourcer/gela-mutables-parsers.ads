------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Nodes;
with Gela.Mutables.Compilations;

package Gela.Mutables.Parsers is
   pragma Preelaborate;

   subtype Parser is Gela.Mutables.Compilations.Compilation;

   type Node_Array is array (Positive range <>) of Gela.Nodes.Element;

end Gela.Mutables.Parsers;
