------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Nodes.Lists is

   type Object is interface and Node;

   type Cursor is record
      Internal_Item : Gela.Nodes.Element;
   end record;

   function Has_Element (Item : Cursor) return Boolean
     is (Gela.Types.">" (Item.Internal_Item.Payload, 0));

   function Element (Item : Cursor) return Gela.Nodes.Element
     is (Item.Internal_Item);

   not overriding function Head
     (Self    : access Object;
      Payload : Gela.Types.Payload)
      return Cursor is abstract;

   not overriding procedure Next
     (Self     : access Object;
      Payload  : Gela.Types.Payload;
      Position : in out Cursor) is abstract;

   not overriding procedure Append
     (Self     : access Object;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element) is abstract;

   not overriding procedure Prepend
     (Self     : access Object;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element) is abstract;

end Gela.Nodes.Lists;
