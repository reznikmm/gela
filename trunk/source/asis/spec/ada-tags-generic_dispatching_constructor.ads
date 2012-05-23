------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $TenDRA: ada-tags-generic_dispatching_constructor.ads 2651 2008-05-29 08:12:10Z maxr $

generic
    type T (<>) is abstract tagged limited private;
    type Parameters (<>) is limited private;

    with function Constructor (Params : not null access Parameters)
           return T is abstract;

function Ada.Tags.Generic_Dispatching_Constructor
 (The_Tag : in Tag;
  Params  : not null access Parameters)
    return T'Class;
pragma Preelaborate (Ada.Tags.Generic_Dispatching_Constructor);
pragma Convention (Intrinsic, Ada.Tags.Generic_Dispatching_Constructor);
