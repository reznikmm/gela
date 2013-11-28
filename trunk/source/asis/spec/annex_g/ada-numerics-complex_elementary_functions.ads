------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: $ $Date: $

with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

package Ada.Numerics.Complex_Elementary_Functions is
  new Ada.Numerics.Generic_Complex_Elementary_Functions
       (Ada.Numerics.Complex_Types);
