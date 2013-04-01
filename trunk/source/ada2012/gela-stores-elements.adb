------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;

package body Gela.Stores.Elements is

   Reference : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
   pragma Unreferenced (Reference);

   ----------
   -- Free --
   ----------

   procedure Free
     (Self    : access Element'Class;
      Payload : in out Gela.Types.Payload)
   is
      Size : constant Positive := Self.Size (Payload);
   begin
      Self.Compilation.Store.Free (Index (Payload), Size);
      Payload := 0;
   end Free;

end Gela.Stores.Elements;
