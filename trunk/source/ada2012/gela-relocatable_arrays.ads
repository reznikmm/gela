------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Relocatable_Arrays is

   type Element is mod 2 ** 32;
   type Index is mod 2 ** 32;

   type Relocatable_Array is limited private;

   function Get (Buffer : Relocatable_Array; Position : Index) return Element
     with Inline;

   procedure Set
     (Buffer   : in out Relocatable_Array;
      Position : Index;
      Value    : Element)
     with Inline;

   function Last (Buffer : Relocatable_Array) return Index;

private

   type Element_Array is array (Index range <>) of Element;
   type Element_Array_Access is access all Element_Array;

   type Relocatable_Array is limited record
      Last : Index := 0;
      Data : Element_Array_Access;
   end record;

end Gela.Relocatable_Arrays;
