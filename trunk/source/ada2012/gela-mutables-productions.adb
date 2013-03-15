------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Properties;
with Gela.Relocatable_Arrays;
with Gela.Mutables.Compilations;

package body Gela.Mutables.Productions is

   Reference : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
   pragma Unreferenced (Reference);

   -----------
   -- Child --
   -----------

   overriding function Child
     (Self    : access Production;
      Payload : Gela.Types.Payload;
      Index   : Positive)
      return Gela.Types.Payload
   is
      use type Gela.Relocatable_Arrays.Index;

      Element : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);

      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get  --  Pointer to child
          (Self.Compilation.Store,
           Element + Gela.Properties.Reserved - 1 +
             Gela.Relocatable_Arrays.Index (Index));
   begin
      if Index > Self.Last_Child (Payload) then
         raise Constraint_Error;
      end if;

      return Gela.Types.Payload (Value);
   end Child;

   ----------------
   -- Last_Child --
   ----------------

   overriding function Last_Child
     (Self    : access Production;
      Payload : Gela.Types.Payload)
      return Natural
   is
      Size : constant Gela.Relocatable_Arrays.Index :=
        Gela.Properties.Size (Self.Production_Index (Payload));
   begin
      return Natural (Size) - Gela.Properties.Reserved;
   end Last_Child;

   ----------------------
   -- Production_Index --
   ----------------------

   not overriding function Production_Index
     (Self    : access Production;
      Payload : Gela.Types.Payload) return Gela.Grammars.Production_Index is
   begin
      return Gela.Grammars.Production_Index (Self.Tag (Payload));
   end Production_Index;

   ---------------
   -- Set_Child --
   ---------------

   overriding procedure Set_Child
     (Self    : access Production;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload)
   is
      use type Gela.Relocatable_Arrays.Index;
      use type Gela.Relocatable_Arrays.Element;

      Element : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
   begin
      if Index > Self.Last_Child (Payload) then
         raise Constraint_Error;
      end if;

      Gela.Relocatable_Arrays.Set  --  Pointer to child
        (Self.Compilation.Store,
         Element + Gela.Properties.Reserved - 1 +
           Gela.Relocatable_Arrays.Index (Index),
         Gela.Relocatable_Arrays.Element (Value));
   end Set_Child;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Self    : access Production;
      Payload : Gela.Types.Payload) return Natural
   is
      Value : constant Gela.Relocatable_Arrays.Index :=
        Gela.Properties.Size (Self.Production_Index (Payload));
   begin
      return Natural (Value);
   end Size;

end Gela.Mutables.Productions;
