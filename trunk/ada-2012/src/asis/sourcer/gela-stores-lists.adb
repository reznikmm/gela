------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Stores.Lists is

   use type Gela.Types.Payload;

   function Get_Tail
     (Self : access List'Class;
      List : Gela.Types.Payload) return Gela.Types.Payload;

   function Get_Next
     (Self : access List'Class;
      Item : Gela.Types.Payload) return Gela.Types.Payload;

   procedure Set_Tail
     (Self : access List'Class;
      List  : Gela.Types.Payload;
      Value : Gela.Types.Payload);

   procedure Set_Next
     (Self : access List'Class;
      Item  : Gela.Types.Payload;
      Value : Gela.Types.Payload);

   Next_Offset : constant := 2;

   package Offset is
      Tag       : constant := 0;
      Count     : constant := Tag + 1;
      Tail      : constant := Count + 1;
      Length    : constant := Tail + 1;
   end Offset;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self     : access List;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element)
   is
      Tail : constant Gela.Types.Payload := Get_Tail (Self, Payload);
   begin
      if Item.its.all not in Lists.Item'Class then
         raise Constraint_Error;
      end if;

      if Tail = 0 then
         Set_Tail (Self, Payload, Item.Payload);
         Set_Next (Self, Item.Payload, Item.Payload);
      else
         Set_Next (Self, Item.Payload, Get_Next (Self, Tail));
         Set_Next (Self, Tail, Item.Payload);
         Set_Tail (Self, Payload, Item.Payload);
      end if;

      Self.Set
        (Index (Payload) + Offset.Length,
         Self.Get (Index (Payload) + Offset.Length) + 1);
   end Append;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Self    : access List;
      Payload : Gela.Types.Payload;
      Index   : Positive)
      return Gela.Types.Payload
   is
      Length : constant Natural := Self.Last_Child (Payload);
   begin
      if Index = Length then
         return Get_Tail (Self, Payload);
      elsif Index > Length then
         raise Constraint_Error;
      elsif Self.Last_List /= Payload or Self.Last_Index > Index then
         Self.Last_List  := Payload;
         Self.Last_Index := 1;
         Self.Last_Item  := Get_Payload (Self.Head (Payload));
      end if;

      while Self.Last_Index /= Index loop
         Self.Last_Index := Self.Last_Index + 1;
         Self.Last_Item := Get_Next (Self, Self.Last_Item);
      end loop;

      return Self.Last_Item;
   end Child;

   --------------
   -- Get_Next --
   --------------

   function Get_Next
     (Self : access List'Class;
      Item : Gela.Types.Payload) return Gela.Types.Payload
   is
      Value : constant Element := Self.Get (Index (Item) + Next_Offset);
   begin
      return Gela.Types.Payload (Value);
   end Get_Next;

   --------------
   -- Get_Tail --
   --------------

   function Get_Tail
     (Self : access List'Class;
      List : Gela.Types.Payload) return Gela.Types.Payload
   is
      Value : constant Element := Self.Get (Index (List) + Offset.Tail);
   begin
      return Gela.Types.Payload (Value);
   end Get_Tail;

   ----------
   -- Head --
   ----------

   function Head
     (Self    : access List;
      Payload : Gela.Types.Payload)
      return Item_Record
   is
      Tail : constant Gela.Types.Payload := Get_Tail (Self, Payload);
   begin
      if Tail = 0 then
         return Null_Item;
      else
         declare
            Next : constant Gela.Types.Payload := Get_Next (Self, Tail);
         begin
            return From_Element
              ((its     =>  Self.To_Node (Next),
                Payload => Next));
         end;
      end if;
   end Head;

   ----------------
   -- Last_Child --
   ----------------

   overriding function Last_Child
     (Self    : access List;
      Payload : Gela.Types.Payload)
      return Natural
   is
      Length : constant Element := Self.Get (Index (Payload) + Offset.Length);
   begin
      return Natural (Length);
   end Last_Child;

   ----------
   -- Next --
   ----------

   procedure Next
     (Self     : access List;
      Payload  : Gela.Types.Payload;
      Position : in out Item_Record)
   is
      Tail : constant Gela.Types.Payload := Get_Tail (Self, Payload);
   begin
      if Get_Payload (Position) = Tail then
         Position := Null_Item;
      else
         declare
            Item  : constant Index := Index (Get_Payload (Position));
            Value : constant Element := Self.Get (Item + Next_Offset);
            Next : constant Gela.Types.Payload := Gela.Types.Payload (Value);
         begin
            Position := From_Element
              ((its     =>  Self.To_Node (Next),
                Payload => Next));
         end;
      end if;
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self     : access List;
      Payload  : Gela.Types.Payload;
      Item     : Gela.Nodes.Element)
   is
      Tail : constant Gela.Types.Payload := Get_Tail (Self, Payload);
   begin
      if Tail = 0 then
         Self.Append (Payload, Item);
         return;
      end if;

      declare
         Head : constant Gela.Types.Payload := Get_Next (Self, Tail);
      begin
         Set_Next (Self, Item.Payload, Head);
         Set_Next (Self, Tail, Item.Payload);
      end;

      Self.Set
        (Index (Payload) + Offset.Length,
         Self.Get (Index (Payload) + Offset.Length) + 1);
   end Prepend;

   ---------------
   -- Set_Child --
   ---------------

   overriding procedure Set_Child
     (Self    : access List;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Payload);
      pragma Unreferenced (Value);
   begin
      raise Program_Error with "Unimplemented procedure Set_Child";
   end Set_Child;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
     (Self : access List'Class;
      Item  : Gela.Types.Payload;
      Value : Gela.Types.Payload) is
   begin
      Self.Set (Index (Item) + Next_Offset, Element (Value));
   end Set_Next;

   --------------
   -- Set_Tail --
   --------------

   procedure Set_Tail
     (Self : access List'Class;
      List  : Gela.Types.Payload;
      Value : Gela.Types.Payload) is
   begin
      Self.Set (Index (List) + Offset.Tail, Element (Value));
   end Set_Tail;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Self    : access List;
      Payload : Gela.Types.Payload)
      return Natural
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Payload);
   begin
      return Offset.Length + 1;
   end Size;

   ----------------
   -- Visit_Each --
   ----------------

   overriding procedure Visit_Each
     (Self    : access List;
      Payload : Gela.Types.Payload;
      Visiter : in out Gela.Nodes.Visiters.Visiter'Class)
   is
      Length : constant Natural := Self.Last_Child (Payload);
      Next   : Gela.Types.Payload := Get_Tail (Self, Payload);
      Item   : Gela.Nodes.Visitable_Node_Access;
   begin
      for J in 1 .. Length loop
         Next := Get_Next (Self, Next);
         Item := Gela.Nodes.Visitable_Node_Access (Self.To_Node (Next));
         Item.Visit (Next, Visiter);
      end loop;
   end Visit_Each;

end Gela.Stores.Lists;
