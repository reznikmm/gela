------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases;

package Gela.Test_Iterators is

   type Iterator is abstract tagged null record;

   procedure Start (Self : in out Iterator) is abstract;

   function Has_More_Tests (Self : Iterator) return Boolean is abstract;

   procedure Next
     (Self : in out Iterator;
      Test : out Gela.Test_Cases.Test_Case_Access) is abstract;

end Gela.Test_Iterators;
