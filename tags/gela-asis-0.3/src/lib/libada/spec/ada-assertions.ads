------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-assertions.ads 2651 2008-05-29 08:12:10Z maxr $

package Ada.Assertions is

   pragma Pure (Assertions);

   Assertion_Error : exception;

   procedure Assert (Check : in Boolean);

   procedure Assert (Check : in Boolean; Message : in String);

end Ada.Assertions;
