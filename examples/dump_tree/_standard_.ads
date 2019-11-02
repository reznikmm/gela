package Standard is
   type Boolean is (False, True);
   type Integer is range -2 ** 31 .. 2 ** 31 - 1;
   subtype Natural is Integer range 0 .. Integer'Last;
   subtype Positive is Integer range 1 .. Integer'Last;

   type Float is digits 6
     range -16#0.FFFF_FF#E32 .. 16#0.FFFF_FF#E32;

   type Character is ('x');
   type String is array (Positive range <>) of Character;

   type Duration is delta 0.000000001
     range -9223372036.85477580 .. 9223372036.854775807;

   Constraint_Error : exception;
end Standard;
