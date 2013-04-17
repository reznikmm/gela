------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Nodes.Lists;
with Gela.Types;
with Gela.Stores.Nodes;

package Gela.Stores.Lists is

   type List is
     new Gela.Stores.Nodes.Node and Gela.Nodes.Lists.Object with private;

   overriding function Head
     (Self    : access List;
      Payload : Gela.Types.Payload)
      return Gela.Nodes.Lists.Cursor;

   overriding procedure Next
     (Self     : access List;
      Payload  : Gela.Types.Payload;
      Position : in out Gela.Nodes.Lists.Cursor);

   overriding procedure Append
     (Self    : access List;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element);

   overriding procedure Prepend
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

   type List is new Gela.Stores.Nodes.Node and Gela.Nodes.Lists.Object with
   record
      Last_List  : Gela.Types.Payload := 0;
      Last_Index : Positive;
      Last_Item  : Gela.Types.Payload;
   end record;

end Gela.Stores.Lists;
