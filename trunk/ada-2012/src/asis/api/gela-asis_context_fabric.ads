------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Context_Fabrics;

package Gela.Asis_Context_Fabric is

   function Get return Gela.Context_Fabrics.Context_Fabric_Access;
   procedure Set (Value : Gela.Context_Fabrics.Context_Fabric_Access);

end Gela.Asis_Context_Fabric;
