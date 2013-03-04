------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Symbol_Tables;

package Gela.Compilations.Mutables.Symbol_Tables is

   type Symbol_Table is new Gela.Symbol_Tables.Symbol_Table
     with private;

   overriding procedure Copy
     (Self     : in out Symbol_Table;
      Payload  : Gela.Elements.Payload;
      Target   : out Gela.Elements.Element);

   overriding procedure Append
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Elements.Payload;
      Name    : Gela.Elements.Symbol_Tables.Symbol;
      Value   : Gela.Elements.Element);

   overriding function Find
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Elements.Payload;
      Name    : Gela.Elements.Symbol_Tables.Symbol)
      return Gela.Elements.Element;

private

   type Tree;
   type Tree_Access is not null access all Tree;

   function Empty_Tree return Tree_Access;

   type Symbol_Table is new Gela.Elements.Symbol_Tables.Symbol_Table
   with record
      Tree : Tree_Access := Empty_Tree;
   end record;

end Gela.Compilations.Mutables.Symbol_Tables;
