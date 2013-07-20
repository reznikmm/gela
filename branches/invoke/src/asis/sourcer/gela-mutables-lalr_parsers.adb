------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with Gela.Mutables.Compilations;
pragma Unreferenced (Gela.Mutables.Compilations);

package body Gela.Mutables.LALR_Parsers is

   procedure Free is new Ada.Unchecked_Deallocation (Stack, Stack_Access);

   -------------
   -- Grammar --
   -------------

   function Grammar
     (Self : access Parser) return Gela.Grammars.Grammar_Access is
   begin
      return Self.Grammar;
   end Grammar;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : access Parser;
      Grammar : Gela.Grammars.Grammar_Access;
      Table   : Gela.Grammars.LR_Tables.Table_Access) is
   begin
      Self.Grammar := Grammar;
      Self.Table := Table;
      Self.Stack := new Stack'(Length => 64,
                               Top    => 0,
                               State  => <>,
                               Node   => <>);
   end Initialize;

   procedure On_Reduce
     (Self  : access Parser;
      Prod  : Gela.Grammars.Production_Index;
      Nodes : in out Node_Array) is separate;

   -----------
   -- Parse --
   -----------

   procedure Parse (Self : access Parser) is
      use type Gela.Grammars.LR.State_Index;
      use Gela.Grammars.LR_Tables;

      Token  : Grammars.Terminal_Count;
      Prod   : Grammars.Production_Index;
      Shift  : Gela.Grammars.LR.State_Count;
      Reduce : Gela.Grammars.LR_Tables.Reduce_Iterator;
   begin
      Self.Top := 1;       --  Starting state
      Self.Stack.Top := 0; --  Clear stack

      Token := Self.Compilation.Lexer.Next;
      loop
         Shift := Self.Table.Shift (Self.Top, Token);
         Reduce := Self.Table.Reduce (Self.Top, Token);

         if Shift /= 0 then
            Push
              (Self.Stack,
               Self.Top,
               (its     => Self.Compilation.Store.Fabric.Token'Access,
                Payload => Self.Compilation.Lexer.Last_Token));

            Self.Top := Shift;

            Token := Self.Compilation.Lexer.Next;
         elsif not Is_Empty (Reduce) then
            Prod := Production (Reduce);

            declare
               Length : constant Positive := 1 +
                 Positive (Self.Grammar.Production (Prod).Last) -
                 Natural (Self.Grammar.Production (Prod).First);

               subtype First_Nodes is Node_Array (1 .. Length);
               S : not null Stack_Access renames Self.Stack;
            begin
               S.Top := S.Top - Length + 1;

               Self.On_Reduce
                 (Prod,
                  First_Nodes
                    (S.Node (S.Top .. S.Top + Length - 1)));

               Shift := S.State (S.Top);

               Self.Top := Self.Table.Shift
                 (Shift, Self.Grammar.Production (Prod).Parent);
            end;
         elsif Self.Table.Finish (Self.Top) then
            Self.Compilation.Root := Self.Stack.Node (Self.Stack.Top);
            exit;
         else
            --  Report error here
            exit;
         end if;
      end loop;
   end Parse;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self  : in out Stack_Access;
      State : Gela.Grammars.LR.State_Index;
      Node  : Gela.Nodes.Element)
   is
   begin
      if Self.Top = Self.Length then
         declare
            Old : Stack_Access := Self;
         begin
            Self := new Stack'(Length => Old.Length * 2,
                               Top    => Old.Top,
                               State  => <>,
                               Node   => <>);
            Self.State (1 .. Old.Top) := Old.State;
            Self.Node (1 .. Old.Top) := Old.Node;
            Free (Old);
         end;
      end if;

      Self.Top := Self.Top + 1;
      Self.State (Self.Top) := State;
      Self.Node (Self.Top) := Node;
   end Push;

end Gela.Mutables.LALR_Parsers;
