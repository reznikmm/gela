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
   pragma Preelaborate;

   type Symbol_Set is interface;

   not overriding procedure Append
     (Self   : in out Symbol_Set;
      Value  : League.Strings.Universal_String;
      Result : out Gela.Types.Symbol) is abstract;

   not overriding function Get
     (Self   : in out Symbol_Set;
      Value  : League.Strings.Universal_String)
      return Gela.Types.Symbol is abstract;
   --  If Value not in Self, return 0

   not overriding function Value
     (Self   : in out Symbol_Set;
      Name   : Gela.Types.Symbol)
      return League.Strings.Universal_String is abstract;

   not overriding procedure Join
     (Self     : in out Symbol_Set;
      Prefix   : Gela.Types.Symbol;
      Selector : Gela.Types.Symbol;
      Result   : out Gela.Types.Symbol) is abstract;

   not overriding function Prefix
     (Self   : in out Symbol_Set;
      Name   : Gela.Types.Symbol)
      return Gela.Types.Symbol is abstract;

end Gela.Symbol_Sets;
