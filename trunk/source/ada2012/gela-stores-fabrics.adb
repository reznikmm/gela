------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;

package body Gela.Stores.Fabrics is

   Reference : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
   pragma Unreferenced (Reference);

   Token_Tag : constant := 0;

   -----------------------
   -- Create_Production --
   -----------------------

   function Create_Production
     (Self       : access Fabric;
      Production : Gela.Grammars.Production_Index)
      return Gela.Types.Payload
   is
      Item : Index;
      Size : constant Positive := Elements.Element_Access
        (Self.Map (Positive (Production))).Size (0);
   begin
      Item := Self.Compilation.Store.Allocate (Size);
      Self.Compilation.Store.Set (Item, Element (Production));  --  Tag
      Self.Compilation.Store.Set (Item + 1, 1);      --  Count

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

      Item := Self.Compilation.Store.Allocate (Size);
      Self.Compilation.Store.Set
        (Item, Element (NT) + Base_Fabrics.Last_Production);  --  Tag
      Self.Compilation.Store.Set (Item + 1, 1);      --  Count
      Self.Compilation.Store.Set (Item + 2, Size);   --  Size

      return Gela.Types.Payload (Item);
   end Create_Switch;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token (Self : access Fabric) return Gela.Types.Payload is
      Item : Index;
   begin
      Item := Self.Compilation.Store.Allocate (Self.Token.Size (0));
      Self.Compilation.Store.Set (Item, Token_Tag);  --  Tag
      Self.Compilation.Store.Set (Item + 1, 1);      --  Count
      return Gela.Types.Payload (Item);
   end Create_Token;

   ----------------
   -- To_Element --
   ----------------

   function To_Element
     (Self    : access Fabric;
      Payload : Gela.Types.Payload)
      return Gela.Stores.Elements.Element_Access is
   begin
      return Gela.Stores.Elements.Element_Access (Self.To_Node (Payload));
   end To_Element;

   -------------
   -- To_Node --
   -------------

   function To_Node
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Nodes.Node_Access
   is
      Item : constant Element := Self.Compilation.Store.Get (Index (Payload));
   begin
      return Self.Map (Natural (Item));
   end To_Node;

end Gela.Stores.Fabrics;
