------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Test_Iterators.Append is

   ---------
   -- "+" --
   ---------

   function "+"
     (Left, Right : Gela.Test_Iterators.Iterator'Class)
      return Iterator
   is
      L : constant Iterator_Access :=
        new Gela.Test_Iterators.Iterator'Class'(Left);
      R : constant Iterator_Access :=
        new Gela.Test_Iterators.Iterator'Class'(Right);
   begin
      return (Append.Left, (L, R));
   end "+";

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Iterator) is
   begin
      Self.Side := Left;
      Self.List (Left).Start;
      Self.List (Right).Start;
   end Start;

   --------------------
   -- Has_More_Tests --
   --------------------

   function Has_More_Tests (Self : Iterator) return Boolean is
   begin
      return Self.List (Self.Side).Has_More_Tests;
   end Has_More_Tests;

   ----------
   -- Next --
   ----------

   procedure Next
     (Self : in out Iterator;
      Test : out Gela.Test_Cases.Test_Case_Access)
   is
   begin
      Self.List (Self.Side).Next (Test);

      if Self.Side = Left and then
        not Self.List (Self.Side).Has_More_Tests
      then
         Self.Side := Right;
      end if;
   end Next;

end Gela.Test_Iterators.Append;
