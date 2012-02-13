------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-complex_text_io.ads 2651 2008-05-29 08:12:10Z maxr $

with Ada.Numerics.Complex_Types;
with Ada.Text_IO.Complex_IO;

package Ada.Complex_Text_IO is
  new Ada.Text_IO.Complex_IO (Ada.Numerics.Complex_Types);
