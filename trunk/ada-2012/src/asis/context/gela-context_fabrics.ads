------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;
with Gela.Errors;

package Gela.Context_Fabrics is
   pragma Preelaborate;

   type Context_Fabric is interface;
   type Context_Fabric_Access is access all Context_Fabric'Class;

   procedure Create_Context
     (Self     : in out Context_Fabric;
      On_Error : not null Gela.Errors.Error_Handler_Access;
      Result   : out Types.Context_Access) is abstract;

end Gela.Context_Fabrics;
