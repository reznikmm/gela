with Asis.Gela.Contexts;
with Asis.Gela.Current_State;

package body Asis.Extensions.Errors is

   -----------------------
   -- Set_Error_Handler --
   -----------------------

   procedure Set_Error_Handler
     (Context : in out Asis.Context;
      Handler : in     Error_Handler_Access)
   is
      Contexts : Asis.Gela.Contexts.Context_List renames
        Asis.Gela.Current_State.Contexts;
   begin
      if Context.Index > 0 then
        Gela.Contexts.Set_Error_Handler (Contexts (Context.Index), Handler);
      end if;
   end Set_Error_Handler;

end Asis.Extensions.Errors;
