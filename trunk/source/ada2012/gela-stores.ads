------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Stores is

   type Store is tagged limited private;
   type Store_Access is access all Store;

private

   type Element is mod 2 ** 32;
   type Index is mod 2 ** 32;

   type Element_Array is array (Index range <>) of Element;
   type Element_Array_Access is access all Element_Array;

   type Store is tagged limited record
      Last      : Index := 1;
      Data      : Element_Array_Access;
      Free_List : Element_Array_Access;
   end record;

   function Get (Self : Store; Position : Index) return Element with Inline;

   procedure Set
     (Self     : in out Store;
      Position : Index;
      Value    : Element) with Inline;

   function Allocate
     (Self : in out Store;
      Size : Natural) return Index;

   procedure Free
     (Self    : in out Store;
      Element : Index;
      Size    : Natural);

end Gela.Stores;
