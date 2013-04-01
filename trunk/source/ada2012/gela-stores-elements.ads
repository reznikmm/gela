------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;
with Gela.Mutables;

package Gela.Stores.Elements is

   type Element (Compilation : Gela.Mutables.Mutable_Compilation_Access) is
     abstract tagged null record;

   procedure Free
     (Self    : access Element'Class;
      Payload : in out Gela.Types.Payload);

   function Size
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Natural is abstract;

   type Element_Access is access all Element'Class;

end Gela.Stores.Elements;
