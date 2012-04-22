------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Iterators;
with League.Strings;

package Gela.Bitten_Report is

   procedure Generate
     (Iterator : in out Test_Iterators.Iterator;
      Result   : out League.Strings.Universal_String);
   --  Fill bitten report file with execution's result.

end Gela.Bitten_Report;
