------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Grammars;
with Gela.Mutables.Elements;
with Gela.Types;

package Gela.Mutables.Productions is

   type Production is
     new Gela.Mutables.Elements.Element with null record;

   not overriding function Production_Index
     (Self    : access Production;
      Payload : Gela.Types.Payload) return Gela.Grammars.Production_Index;

   overriding function Last_Child
     (Self    : access Production;
      Payload : Gela.Types.Payload) return Natural;

   overriding function Child
     (Self    : access Production;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Types.Payload;

   overriding procedure Set_Child
     (Self    : access Production;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload);

   overriding function Size
     (Self    : access Production;
      Payload : Gela.Types.Payload) return Natural;

end Gela.Mutables.Productions;
