------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-strings-unbounded-hash.ads 2651 2008-05-29 08:12:10Z maxr $

with Ada.Containers;

function Ada.Strings.Unbounded.Hash (Key : in Unbounded_String)
  return Ada.Containers.Hash_Type;

pragma Preelaborate (Hash);
