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
      Size : Positive;
   begin
      case Production is
         when 1 =>
            Size := Self.P1.Size (0);
         when others =>
            raise Constraint_Error;
      end case;

      Item := Self.Compilation.Store.Allocate (Size);
      Self.Compilation.Store.Set (Item, Element (Production));  --  Tag
      Self.Compilation.Store.Set (Item + 1, 1);      --  Count

      return Gela.Types.Payload (Item);
   end Create_Production;

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
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Fabric) is
   begin
      Self.Map (1) := Self.P1'Access;
   end Initialize;

   ----------------
   -- To_Element --
   ----------------

   function To_Element
     (Self    : access Fabric;
      Payload : Gela.Types.Payload)
      return Gela.Stores.Elements.Element_Access
   is
      Item : constant Index := Index (Payload);
   begin
      case Self.Compilation.Store.Get (Item) is
         when Token_Tag =>
            return Self.Token'Access;
         when 1 =>
            return Self.P1'Access;
         when others =>
            raise Constraint_Error;
      end case;
   end To_Element;

   -------------
   -- To_Node --
   -------------

   function To_Node
     (Self    : access Fabric;
      Payload : Gela.Types.Payload) return Gela.Nodes.Node_Access is
   begin
      return Gela.Nodes.Node_Access (Self.To_Element (Payload));
   end To_Node;

end Gela.Stores.Fabrics;
