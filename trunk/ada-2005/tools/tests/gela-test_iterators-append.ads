------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases;
with Gela.Test_Iterators;

package Gela.Test_Iterators.Append is

   type Iterator is new Gela.Test_Iterators.Iterator with private;

   function "+"
     (Left, Right : Gela.Test_Iterators.Iterator'Class) return Iterator;
   --  Concatenate two iterators.

   procedure Start (Self : in out Iterator);

   function Has_More_Tests (Self : Iterator) return Boolean;

   procedure Next
     (Self : in out Iterator;
      Test : out Gela.Test_Cases.Test_Case_Access);

private

   type Iterator_Access is access all Gela.Test_Iterators.Iterator'Class;

   type Side_Kind is (Left, Right);
   type Iterator_Access_Array is array (Side_Kind) of Iterator_Access;

   type Iterator is new Gela.Test_Iterators.Iterator with record
      Side  : Side_Kind;
      List  : Iterator_Access_Array;
   end record;

end Gela.Test_Iterators.Append;
