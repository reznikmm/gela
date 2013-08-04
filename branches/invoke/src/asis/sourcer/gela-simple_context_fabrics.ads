------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Context_Fabrics;
with Gela.Errors;
with Gela.Types;

package Gela.Simple_Context_Fabrics is

   type Simple_Context_Fabric is
     new Gela.Context_Fabrics.Context_Fabric with null record;

   procedure Create_Context
     (Self     : in out Simple_Context_Fabric;
      On_Error : Gela.Errors.Error_Handler_Access;
      Result   : out Gela.Types.Context_Access);

end Gela.Simple_Context_Fabrics;
