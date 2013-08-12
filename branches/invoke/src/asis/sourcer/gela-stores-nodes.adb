------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;
pragma Unreferenced (Gela.Mutables.Compilations);

package body Gela.Stores.Nodes is

   --  Tag_Offset := 0;
   Count_Offset : constant := 1;
   Next_Offset : constant := 2;
   Reserved    : constant := Next_Offset;
   --  First_child_Offset := 3;

   -----------
   -- Child --
   -----------

   function Child
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Types.Payload is
   begin
      return Gela.Types.Payload (Self.Property (Payload, Index));
   end Child;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (Self    : access Node'Class;
      Payload : in out Gela.Types.Payload)
   is
      use type Gela.Types.Payload;

      Child : Gela.Types.Payload;
      Item  : constant Index := Index (Payload);
      Count : constant Element := Self.Get (Item + Count_Offset);
   begin
      if Payload = 0 then
         return;
      elsif Count = 1 then
         for J in 1 .. Self.Last_Child (Payload) loop
            Child := Self.Child (Payload, J);
            Self.Dereference (Child);
         end loop;

         Self.Free (Payload);
      else
         Self.Set (Item + Count_Offset, Count - 1);
      end if;
   end Dereference;

   --------------
   -- Property --
   --------------

   function Property
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Stores.Element
   is
      Child_Index : constant Stores.Index :=
        Stores.Index (Payload) + Reserved + Stores.Index (Index);
   begin
      return Self.Get (Child_Index);
   end Property;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self    : access Node'Class;
      Payload : Gela.Types.Payload)
   is
      use type Gela.Types.Payload;

      Item  : constant Index := Index (Payload);
      Count : constant Element := Self.Get (Item + Count_Offset);
   begin
      if Payload /= 0 then
         Self.Set (Item + Count_Offset, Count + 1);
      end if;
   end Reference;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload)
   is
      Child : Element := Self.Property (Payload, Index);
   begin
      if Child /= 0 then
         Self.Dereference (Gela.Types.Payload (Child));
      end if;

      Self.Set_Property (Payload, Index, Element (Value));
      Self.Reference (Value);
   end Set_Child;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Self    : access Node;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Stores.Element)
   is
      Child_Index : constant Stores.Index :=
        Stores.Index (Payload) + Reserved + Stores.Index (Index);
   begin
      Self.Set (Child_Index, Value);
   end Set_Property;

end Gela.Stores.Nodes;
