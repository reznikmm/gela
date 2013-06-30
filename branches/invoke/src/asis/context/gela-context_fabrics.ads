------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;

package Gela.Context_Fabrics is
   pragma Preelaborate;

   type Context_Fabric is interface;
   type Context_Fabric_Access is access all Context_Fabric'Class;

   type On_Error_Callback is access procedure (Text : Wide_String);

   procedure Create_Context
     (Self     : in out Context_Fabric;
      On_Error : On_Error_Callback;
      Result   : out Types.Context_Access) is abstract;

end Gela.Context_Fabrics;
