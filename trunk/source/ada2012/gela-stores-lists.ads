------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Nodes;
with Gela.Types;
with Gela.Stores.Nodes;

generic
   type Node_List is interface and Gela.Nodes.Node;
   type Item is interface and Gela.Nodes.Node;
   type Item_Record is private;

   Null_Item : Item_Record;

   with function From_Element (X : Gela.Nodes.Element) return Item_Record;

   with function Get_Payload (Object  : Item_Record) return Gela.Types.Payload;

   with function Head
     (Self    : access Node_List;
      Payload : Gela.Types.Payload)
      return Item_Record is abstract;

   with procedure Next
     (Self     : access Node_List;
      Payload  : Gela.Types.Payload;
      Position : in out Item_Record) is abstract;

   with procedure Append
     (Self     : access Node_List;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element) is abstract;

   with procedure Prepend
     (Self     : access Node_List;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element) is abstract;
package Gela.Stores.Lists is
   pragma Preelaborate;
   pragma Elaborate_Body;

   type List is
     new Gela.Stores.Nodes.Node and Node_List with private;

   type List_Access is access all List;

   function Head
     (Self    : access List;
      Payload : Gela.Types.Payload)
      return Item_Record;

   procedure Next
     (Self     : access List;
      Payload  : Gela.Types.Payload;
      Position : in out Item_Record);

   procedure Append
     (Self    : access List;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element);

   procedure Prepend
     (Self    : access List;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element);

   overriding function Last_Child
     (Self    : access List;
      Payload : Gela.Types.Payload) return Natural;

   overriding function Size
     (Self    : access List;
      Payload : Gela.Types.Payload) return Natural;

   overriding function Child
     (Self    : access List;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Types.Payload;

   overriding procedure Set_Child
     (Self    : access List;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload);

private

   type List is new Gela.Stores.Nodes.Node and Node_List with record
      Last_List  : Gela.Types.Payload := 0;
      Last_Index : Positive;
      Last_Item  : Gela.Types.Payload;
   end record;

end Gela.Stores.Lists;
