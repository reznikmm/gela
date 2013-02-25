------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with Gela.Elements.Symbol_Tables;

package Gela.Elements.Folded_Sets is

   subtype Symbol is Gela.Elements.Symbol_Tables.Symbol;

   type Folded_Set is abstract tagged limited null record;

   not overriding procedure Append
     (Self   : in out Folded_Set;
      Value  : League.Strings.Universal_String;
      Result : out Symbol) is abstract;

--   not overriding function Value
--     (Self   : in out Folded_Set;
--      Name   : Symbol) return League.Strings.Universal_String is abstract;

end Gela.Elements.Folded_Sets;
