------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-containers.ads 2651 2008-05-29 08:12:10Z maxr $

package Ada.Containers is

   pragma Pure (Containers);

   type Hash_Type is mod implementation-defined;

   type Count_Type is range 0 .. implementation-defined;

end Ada.Containers;
