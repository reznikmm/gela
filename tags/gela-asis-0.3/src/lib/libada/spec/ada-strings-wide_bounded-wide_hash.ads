------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-strings-wide_bounded-wide_hash.ads 2651 2008-05-29 08:12:10Z maxr $

with Ada.Containers;

generic
   with package Bounded is
     new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (<>);

function Ada.Strings.Wide_Bounded.Wide_Hash
  (Key : in Bounded.Bounded_Wide_String)
  return Containers.Hash_Type;

pragma Preelaborate (Wide_Hash);
