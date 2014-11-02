PACKAGE L19 IS

    TYPE L23 IS DIGITS 6;
    FOR L23'SIZE USE 32;

    L620 : CONSTANT L23 := L23'FIRST;

    TYPE L41 IS ARRAY (POSITIVE RANGE <>) OF L23;

    TYPE L140 IS NEW L41(1..2);

    L917 : CONSTANT L140 := (OTHERS => L620);

END L19;

WITH L19;

PACKAGE L239 is

   procedure Main;

END L239;

package body L239 is

   procedure Main is
   begin
      null;
   end Main;
end L239;
