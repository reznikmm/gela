------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------
--  Root type for AST nodes

with Gela.Types;

package Gela.Stores.Nodes is
   pragma Preelaborate;

   type Node is abstract new Gela.Stores.Storable_Element with null record;

   procedure Reference
     (Self    : access Node'Class;
      Payload : Gela.Types.Payload);

   procedure Dereference
     (Self    : access Node'Class;
      Payload : in out Gela.Types.Payload);

   not overriding function Last_Child
     (Self    : access Node;
      Payload : Gela.Types.Payload) return Natural is abstract;

   function Child
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Types.Payload;

   procedure Set_Child
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload);

   function Property
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Stores.Element;

   procedure Set_Property
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Stores.Element);

   pragma Inline (Reference);
   pragma Inline (Dereference);
   pragma Inline (Child);
   pragma Inline (Set_Child);
   pragma Inline (Property);
   pragma Inline (Set_Property);

end Gela.Stores.Nodes;
