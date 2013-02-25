------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Elements.Folded_Sets;
with Gela.Elements.Symbol_Tables;

package Gela.Compilations.Mutables.Folded_Sets is

   type Folded_Set is new Gela.Elements.Folded_Sets.Folded_Set with record
      Compilation : Mutable_Compilation_Access;
   end record;

   overriding procedure Append
     (Self   : in out Folded_Set;
      Value  : League.Strings.Universal_String;
      Result : out Gela.Elements.Symbol_Tables.Symbol);

end Gela.Compilations.Mutables.Folded_Sets;
