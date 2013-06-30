------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Simple_Contexts;

package body Gela.Simple_Context_Fabrics is

   --------------------
   -- Create_Context --
   --------------------

   procedure Create_Context
     (Self     : in out Simple_Context_Fabric;
      On_Error : Gela.Context_Fabrics.On_Error_Callback;
      Result   : out Gela.Types.Context_Access)
   is
      pragma Unreferenced (Self);
   begin
      Result := new Gela.Simple_Contexts.Context (On_Error);
   end Create_Context;

end Gela.Simple_Context_Fabrics;
