------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases;
with League.Strings;

private with Ada.Containers.Ordered_Maps;

package Gela.Test_Iterators is

   type Iterator is tagged private;

   function Create (Path : League.Strings.Universal_String) return Iterator;

   procedure Start (Self : in out Iterator);

   function Has_More_Tests (Self : Iterator) return Boolean;

   procedure Next
     (Self : in out Iterator;
      Test : out Gela.Test_Cases.Test_Case_Access);

private

   package Maps is new Ada.Containers.Ordered_Maps
     (League.Strings.Universal_String,
      Gela.Test_Cases.Test_Case_Access,
      League.Strings."<",
      Gela.Test_Cases."=");

   type Iterator is tagged record
      Map : Maps.Map;
      Next : Maps.Cursor;
   end record;

end Gela.Test_Iterators;
