------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Elements.Symbol_Tables is

   type Symbol is mod 2 ** 31;
   --  Reserve one bit for red/black flag

   type Symbol_Table is abstract new Fly_Weight_Object with null record;

   not overriding procedure Copy
     (Self     : in out Symbol_Table;
      Payload  : Gela.Elements.Payload;
      Target   : out Gela.Elements.Element) is abstract;
   --  Create copy on write of Self into Target. After this any modification
   --  of Self or Target will actually create new copy

   not overriding procedure Append
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Elements.Payload;
      Name    : Symbol;
      Value   : Gela.Elements.Element) is abstract;

   not overriding function Find
     (Self    : in out Symbol_Table;
      Payload : in out Gela.Elements.Payload;
      Name    : Symbol) return Gela.Elements.Element is abstract;

end Gela.Elements.Symbol_Tables;
