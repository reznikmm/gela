------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-numerics-complex_arrays.ads 2651 2008-05-29 08:12:10Z maxr $

with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Real_Arrays;

package Ada.Numerics.Complex_Arrays is
  new Ada.Numerics.Generic_Complex_Arrays (Ada.Numerics.Real_Arrays,
                                           Ada.Numerics.Complex_Types);
