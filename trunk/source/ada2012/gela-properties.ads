------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Relocatable_Arrays;

package Gela.Properties is

   type Global_Kind is (Line, Token);

   type Property_Kind is
     (Value, Line, First, Last, Next, Separator, Symbol, Comment);

   function Property_Index
     (Element  : Global_Kind;
      Property : Property_Kind)
      return Gela.Relocatable_Arrays.Index
      with Inline;

   function Size
     (Element  : Global_Kind)
      return Gela.Relocatable_Arrays.Index
      with Inline;

end Gela.Properties;
