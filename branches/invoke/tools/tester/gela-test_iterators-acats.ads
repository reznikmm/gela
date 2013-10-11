------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
private with Ada.Containers.Doubly_Linked_Lists;

package Gela.Test_Iterators.ACATS is

   type Iterator is new Test_Iterators.Iterator with private;

   function Create
     (Command   : League.Strings.Universal_String;
      List_File : League.Strings.Universal_String;
      ACATS     : League.Strings.Universal_String)
      return Iterator;
   --  Create iterator for enumerating tests containing in Source directory.
   --  Build point to directory where tests will be build.

   procedure Start (Self : in out Iterator);

   function Has_More_Tests (Self : Iterator) return Boolean;

   procedure Next
     (Self : in out Iterator;
      Test : out Gela.Test_Cases.Test_Case_Access);

private

   package Lists is new Ada.Containers.Doubly_Linked_Lists
     (Gela.Test_Cases.Test_Case_Access,
      Gela.Test_Cases."=");

   type Iterator is new Gela.Test_Iterators.Iterator with record
      List : Lists.List;
      Next : Lists.Cursor;
   end record;

end Gela.Test_Iterators.ACATS;