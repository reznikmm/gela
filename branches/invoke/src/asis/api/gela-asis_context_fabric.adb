------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Simple_Context_Fabrics;

package body Gela.Asis_Context_Fabric is

   Current : Gela.Context_Fabrics.Context_Fabric_Access;

   ---------
   -- Get --
   ---------

   function Get return Gela.Context_Fabrics.Context_Fabric_Access is
      use type Gela.Context_Fabrics.Context_Fabric_Access;
   begin
      if Current = null then
         Current := new Gela.Simple_Context_Fabrics.Simple_Context_Fabric;
      end if;

      return Current;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Value : Gela.Context_Fabrics.Context_Fabric_Access) is
   begin
      Current := Value;
   end Set;

end Gela.Asis_Context_Fabric;
