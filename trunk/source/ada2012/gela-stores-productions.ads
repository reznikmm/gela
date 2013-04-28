------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Grammars;
with Gela.Stores.Nodes;
with Gela.Types;

package Gela.Stores.Productions is
   pragma Preelaborate;

   type Production is
     abstract new Gela.Stores.Nodes.Node with null record;

   not overriding function Production_Index
     (Self    : access Production;
      Payload : Gela.Types.Payload) return Gela.Grammars.Production_Index;

end Gela.Stores.Productions;
