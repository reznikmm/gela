------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;

package body Gela.Stores.Productions is

   Reference : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
   pragma Unreferenced (Reference);

   ----------------------
   -- Production_Index --
   ----------------------

   not overriding function Production_Index
     (Self    : access Production;
      Payload : Gela.Types.Payload)
      return Gela.Grammars.Production_Index
   is
      Item  : constant Index := Index (Payload);
      Value : constant Element := Self.Compilation.Store.Get (Item);
   begin
      return Gela.Grammars.Production_Index (Value);
   end Production_Index;

end Gela.Stores.Productions;
