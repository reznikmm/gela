------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;

package body Gela.Stores.Nodes is

   Ref : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
   pragma Unreferenced (Ref);

   Count_Offset : constant := 1;

   -----------
   -- Child --
   -----------

   function Child
     (Self    : access Node'Class;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Types.Payload
   is
      Child_Index : constant Stores.Index :=
        Stores.Index (Payload) + Count_Offset + Stores.Index (Index);
      Child : constant Element := Self.Compilation.Store.Get (Child_Index);
   begin
      return Gela.Types.Payload (Child);
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
      Count : constant Element :=
        Self.Compilation.Store.Get (Item + Count_Offset);
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
         Self.Compilation.Store.Set (Item + Count_Offset, Count - 1);
      end if;
   end Dereference;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self    : access Node'Class;
      Payload : Gela.Types.Payload)
   is
      use type Gela.Types.Payload;

      Item  : constant Index := Index (Payload);
      Count : constant Element :=
        Self.Compilation.Store.Get (Item + Count_Offset);
   begin
      if Payload /= 0 then
         Self.Compilation.Store.Set (Item + Count_Offset, Count + 1);
      end if;
   end Reference;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Self    : access Node'Class;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload)
   is
      Child_Index : constant Stores.Index :=
        Stores.Index (Payload) + Count_Offset + Stores.Index (Index);
      Child : Element := Self.Compilation.Store.Get (Child_Index);
   begin
      if Child /= 0 then
         Self.Dereference (Gela.Types.Payload (Child));
      end if;

      Self.Compilation.Store.Set (Child_Index, Element (Value));
      Self.Reference (Value);
   end Set_Child;

end Gela.Stores.Nodes;
