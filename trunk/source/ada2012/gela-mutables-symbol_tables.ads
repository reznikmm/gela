------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Symbol_Tables;
with Gela.Types;

package Gela.Mutables.Symbol_Tables is

   type Symbol_Table is new Gela.Symbol_Tables.Abstract_Symbol_Table
     with private;

   overriding procedure Copy
     (Self     : in out Symbol_Table;
      Payload  : Gela.Types.Payload;
      Target   : out Gela.Types.Symbol_Table);

   overriding procedure Append
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Types.Payload;
      Name    : Gela.Types.Symbol;
      Value   : Gela.Types.Defining_Name);

   overriding function Find
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Types.Payload;
      Name    : Gela.Types.Symbol)
      return Gela.Types.Defining_Name;

private

   type Tree;
   type Tree_Access is not null access all Tree;

   function Empty_Tree return Tree_Access;

   type Symbol_Table is new Gela.Symbol_Tables.Abstract_Symbol_Table
   with record
      Tree : Tree_Access := Empty_Tree;
   end record;

end Gela.Mutables.Symbol_Tables;
