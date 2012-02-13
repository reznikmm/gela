------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-numerics-complex_elementary_functions.ads 2651 2008-05-29 08:12:10Z maxr $

with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

package Ada.Numerics.Complex_Elementary_Functions is
  new Ada.Numerics.Generic_Complex_Elementary_Functions
       (Ada.Numerics.Complex_Types);
