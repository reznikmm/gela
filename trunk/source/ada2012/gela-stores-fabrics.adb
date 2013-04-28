------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Stores.Fabrics is

   Token_Tag : constant := 0;

   -----------------
   -- Create_List --
   -----------------

   overriding function Create_List
     (Self : access Fabric;
      Tag  : Positive)
      return Gela.Types.Payload
   is
      Item : Index;
      Size : constant Positive := 4;
   begin
      Item := Self.Store.Allocate (Size);
      Self.Store.Set (Item, Element (Tag));  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count

      return Gela.Types.Payload (Item);
   end Create_List;

   -----------------------
   -- Create_Production --
   -----------------------

   overriding function Create_Production
     (Self       : access Fabric;
      Production : Gela.Grammars.Production_Index)
      return Gela.Types.Payload
   is
      Item : Index;
      Size : constant Positive := Element_Access
        (Self.Map (Positive (Production))).Size (0);
   begin
      Item := Self.Store.Allocate (Size);
      Self.Store.Set (Item, Element (Production));  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count

      return Gela.Types.Payload (Item);
   end Create_Production;

   -------------------
   -- Create_Switch --
   -------------------

   function Create_Switch
     (Self : access Fabric;
      NT   : Gela.Grammars.Non_Terminal_Index)
      return Gela.Types.Payload
   is
      use type Gela.Nodes.Node_Access;

      Item : Index;
      Size : constant := 5;
   begin
      if Self.Map (Positive (NT) + Base_Fabrics.Last_Production) = null then
         raise Constraint_Error;
      end if;

      Item := Self.Store.Allocate (Size);
      Self.Store.Set
        (Item, Element (NT) + Base_Fabrics.Last_Production);  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count
      Self.Store.Set (Item + 2, Size);   --  Size

      return Gela.Types.Payload (Item);
   end Create_Switch;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token (Self : access Fabric) return Gela.Types.Payload is
      Item : Index;
   begin
      Item := Self.Store.Allocate (Self.Token.Size (0));
      Self.Store.Set (Item, Token_Tag);  --  Tag
      Self.Store.Set (Item + 1, 1);      --  Count
      return Gela.Types.Payload (Item);
   end Create_Token;

   ----------------
   -- To_Element --
   ----------------

   function To_Element
     (Self    : access Fabric;
      Payload : Gela.Types.Payload)
      return Gela.Stores.Element_Access is
   begin
      return Gela.Stores.Element_Access (Self.To_Node (Payload));
   end To_Element;

   -------------
   -- To_Node --
   -------------

   function To_Node
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Nodes.Node_Access
   is
      use type Gela.Types.Payload;
   begin
      if Payload = 0 then
         return null;
      else
         declare
            Item : constant Element := Self.Store.Get (Index (Payload));
         begin
            return Self.Map (Natural (Item));
         end;
      end if;
   end To_Node;

end Gela.Stores.Fabrics;
