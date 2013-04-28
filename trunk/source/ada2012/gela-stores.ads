------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables;
with Gela.Types;
with Gela.Nodes;
with Gela.Grammars;

package Gela.Stores is
   pragma Preelaborate;

   type Store (Compilation : Gela.Mutables.Mutable_Compilation_Access)
     is tagged limited private;
   type Store_Access is access all Store'Class;

   type Abstract_Element (Store : not null Store_Access) is
     abstract tagged private;

   type Element_Access is access all Abstract_Element'Class;

   function Size
     (Self    : access Abstract_Element;
      Payload : Gela.Types.Payload) return Natural is abstract;

   type Abstract_Fabric is abstract tagged limited null record;

   function Create_Production
     (Self       : access Abstract_Fabric;
      Production : Gela.Grammars.Production_Index)
      return Gela.Types.Payload is abstract;

   function Create_List
     (Self : access Abstract_Fabric;
      Tag  : Positive)
      return Gela.Types.Payload is abstract;

private

   type Element is mod 2 ** 32;
   type Index is mod 2 ** 32;

   type Element_Array is array (Index range <>) of Element;
   type Element_Array_Access is access all Element_Array;

   type Store (Compilation : Gela.Mutables.Mutable_Compilation_Access) is
   tagged limited record
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

   type Abstract_Element (Store : not null Store_Access) is
     abstract tagged null record;

   function Get
     (Self     : Abstract_Element'Class;
      Position : Index) return Gela.Stores.Element with Inline;

   procedure Set
     (Self     : Abstract_Element'Class;
      Position : Index;
      Value    : Gela.Stores.Element) with Inline;

   procedure Free
     (Self    : access Abstract_Element'Class;
      Payload : in out Gela.Types.Payload);

   function To_Node
     (Self    : access Abstract_Element'Class;
      Payload : Gela.Types.Payload) return Gela.Nodes.Node_Access;

end Gela.Stores;
