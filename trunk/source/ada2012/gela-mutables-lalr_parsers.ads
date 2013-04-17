------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Nodes;
with Gela.Grammars.LR;
with Gela.Grammars.LR_Tables;

package Gela.Mutables.LALR_Parsers is

   type Parser (Compilation : Mutable_Compilation_Access) is tagged private;

   procedure Parse (Self : access Parser);

   type Grammar_Access is access all Gela.Grammars.Grammar;

   function Grammar (Self : access Parser) return Grammar_Access;

private

   type State_Array is array (Positive range <>) of
     Gela.Grammars.LR.State_Index;

   type Node_Array is array (Positive range <>) of Gela.Nodes.Element;

   type Stack (Length : Positive) is record
      Top   : Natural;
      State : State_Array (1 .. Length);
      Node  : Node_Array (1 .. Length);
   end record;

   type Stack_Access is access all Stack;

   procedure Push
     (Self  : in out Stack_Access;
      State : Gela.Grammars.LR.State_Index;
      Node  : Gela.Nodes.Element);

   type Table_Access is access all Gela.Grammars.LR_Tables.Table;

   type Parser (Compilation : Mutable_Compilation_Access) is tagged record
      Grammar : Grammar_Access;
      Top     : Gela.Grammars.LR.State_Index;
      Table   : Table_Access;
      Stack   : Stack_Access;
   end record;

   procedure On_Reduce
     (Self  : access Parser;
      Prod  : Gela.Grammars.Production_Index;
      Nodes : in out Node_Array);

end Gela.Mutables.LALR_Parsers;
