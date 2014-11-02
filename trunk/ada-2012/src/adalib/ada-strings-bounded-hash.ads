------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision$ $Date$

with Ada.Containers;

generic
   with package Bounded is
     new Ada.Strings.Bounded.Generic_Bounded_Length (<>);

function Ada.Strings.Bounded.Hash (Key : in Bounded.Bounded_String)
  return Containers.Hash_Type;
pragma Preelaborate (Hash);
