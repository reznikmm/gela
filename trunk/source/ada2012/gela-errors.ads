------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Types;

package Gela.Errors is

   type Error_Handler is limited interface;

   procedure Not_In_NFKC_Warning
     (Self        : access Error_Handler;
      Compilation : Gela.Types.Compilation_Access) is abstract;
   --  Text of compilation is not in Normalization Form KC. ARM 4.1/3

end Gela.Errors;
