------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------
--  Symbol_Set contains map form identifier, character literal and operation
--  literal to unique (numeric) symbol. Symbols used to save space and speed up
--  symbol tables.

with League.Strings;
with Gela.Types;

package Gela.Symbol_Sets is

   type Symbol_Set is interface;

   not overriding procedure Append
     (Self   : in out Symbol_Set;
      Value  : League.Strings.Universal_String;
      Result : out Gela.Types.Symbol) is abstract;

--   not overriding function Value
--     (Self   : in out Symbol_Set;
--      Name   : Symbol) return League.Strings.Universal_String is abstract;

end Gela.Symbol_Sets;
