--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Scanner_States is
   pragma Pure;

   type State is mod +86;
   subtype Looping_State is State range 0 .. 63;
   subtype Final_State is State range 36 .. State'Last - 1;

   Error_State : constant State := State'Last;

   Allow_Char : constant State := 0;
   INITIAL : constant State := 35;

   type Character_Class is mod +35;

   type Rule_Index is range 0 .. 38;

end Program.Scanner_States;
