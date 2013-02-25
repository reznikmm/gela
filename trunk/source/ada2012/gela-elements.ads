------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Elements is

   pragma Preelaborate;

   type Fly_Weight_Object is tagged limited null record;
   type Fly_Weight_Object_Access is access all Fly_Weight_Object'Class;

   type Payload is mod 2 ** 32;

   type Element is record
      Object  : Fly_Weight_Object_Access;
      Payload : Gela.Elements.Payload;
   end record;

   type Payload_Array is array (Positive range <>) of Payload;

end Gela.Elements;
