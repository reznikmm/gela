------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;

with Gela.Folded_Sets;
with Gela.Types;

package Gela.Compilations.Mutables.Folded_Sets is

   package Symbol_Maps is new Ada.Containers.Ordered_Maps
     (League.Strings.Universal_String,
      Gela.Types.Symbol,
      League.Strings."<",
      Gela.Types."=");

   Non_ASCII : constant Wide_Wide_Character := Wide_Wide_Character'Succ ('z');

   type Char_to_Symbol_Map is array
     (Wide_Wide_Character range 'a' .. Non_ASCII) of
     Gela.Types.Symbol;

   type Folded_Set is new Gela.Folded_Sets.Folded_Set with record
      Compilation : Mutable_Compilation_Access;
      Symbols     : Symbol_Maps.Map;
      Last        : Char_to_Symbol_Map := (others => 0);
   end record;

   overriding procedure Append
     (Self   : in out Folded_Set;
      Value  : League.Strings.Universal_String;
      Result : out Gela.Types.Symbol);

end Gela.Compilations.Mutables.Folded_Sets;
