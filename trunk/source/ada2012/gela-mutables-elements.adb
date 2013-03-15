with Gela.Mutables.Compilations;
with Gela.Relocatable_Arrays;

package body Gela.Mutables.Elements is

   Reference : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
   pragma Unreferenced (Reference);

   -----------
   -- Count --
   -----------

   function Count
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Natural
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get (Self.Compilation.Store, Index + 1);
   begin
      return Natural (Value);
   end Count;

   --------------------
   -- Free_List_Link --
   --------------------

   function Free_List_Link
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Gela.Types.Payload is
   begin
      return Gela.Types.Payload (Self.Tag (Payload));
   end Free_List_Link;

   ---------------
   -- Set_Count --
   ---------------

   procedure Set_Count
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Value   : Natural)
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
   begin
      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + 1,
         Gela.Relocatable_Arrays.Element (Value));
   end Set_Count;

   ------------------------
   -- Set_Free_List_Link --
   ------------------------

   procedure Set_Free_List_Link
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Value   : Gela.Types.Payload) is
   begin
      Self.Set_Tag (Payload, Natural (Value));
   end Set_Free_List_Link;

   -------------
   -- Set_Tag --
   -------------

   procedure Set_Tag
     (Self    : access Element;
      Payload : Gela.Types.Payload;
      Value   : Natural)
   is
      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
   begin
      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index,
         Gela.Relocatable_Arrays.Element (Value));
   end Set_Tag;

   ---------
   -- Tag --
   ---------

   function Tag
     (Self    : access Element;
      Payload : Gela.Types.Payload) return Natural
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get (Self.Compilation.Store, Index);
   begin
      return Natural (Value);
   end Tag;

end Gela.Mutables.Elements;
