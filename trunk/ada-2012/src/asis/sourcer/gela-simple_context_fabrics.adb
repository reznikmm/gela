------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Application;
with League.Strings;

with Gela.Simple_Contexts;

package body Gela.Simple_Context_Fabrics is

   --------------------
   -- Create_Context --
   --------------------

   procedure Create_Context
     (Self     : in out Simple_Context_Fabric;
      On_Error : not null Gela.Errors.Error_Handler_Access;
      Result   : out Gela.Types.Context_Access)
   is
      pragma Unreferenced (Self);
      Context           : Gela.Simple_Contexts.Context_Access;
      Gela_Include_Path : constant League.Strings.Universal_String :=
        League.Strings.To_Universal_String ("GELA_INCLUDE_PATH");
   begin
         Context := new Gela.Simple_Contexts.Context (On_Error);

         Context.Initialize
           (Default_Path =>
              League.Application.Environment.Value (Gela_Include_Path));

         Result := Gela.Types.Context_Access (Context);
   end Create_Context;

end Gela.Simple_Context_Fabrics;
