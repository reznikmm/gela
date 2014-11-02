------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

package Gela.Bitten_Coverage is

   procedure Generate
     (Build    : League.Strings.Universal_String;
      Result   : out League.Strings.Universal_String);
   --  Fill bitten coverage report file with execution's result.

end Gela.Bitten_Coverage;
