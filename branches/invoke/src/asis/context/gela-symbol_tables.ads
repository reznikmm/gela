------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;

package Gela.Symbol_Tables is

   type Abstract_Symbol_Table is interface;

   not overriding procedure Copy
     (Self     : in out Abstract_Symbol_Table;
      Payload  : Gela.Types.Payload;
      Target   : out Gela.Types.Symbol_Table) is abstract;
   --  Create copy on write of Self into Target. After this any modification
   --  of Self or Target will actually create new copy

   not overriding procedure Append
     (Self    : in out Abstract_Symbol_Table;
      Payload : in out Gela.Types.Payload;
      Name    : Gela.Types.Symbol;
      Value   : Gela.Types.Defining_Name) is abstract;

   not overriding function Find
     (Self    : in out Abstract_Symbol_Table;
      Payload : in out Gela.Types.Payload;
      Name    : Gela.Types.Symbol)
      return Gela.Types.Defining_Name is abstract;

end Gela.Symbol_Tables;
