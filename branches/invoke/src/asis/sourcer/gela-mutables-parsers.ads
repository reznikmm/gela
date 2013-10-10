------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Nodes;

package Gela.Mutables.Parsers is
   pragma Preelaborate;

   type Parser (Compilation : Mutable_Compilation_Access) is null record;

   type Node_Array is array (Positive range <>) of Gela.Nodes.Element;

end Gela.Mutables.Parsers;
