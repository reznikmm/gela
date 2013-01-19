------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Calendars;
with League.Strings;

package Gela.Test_Cases is

   type Test_Case is abstract tagged null record;
   --  Interface to test case.

   type Test_Case_Access is access all Test_Case'Class;

   type Status_Kind is (Success, Failure, Error, Ignore);

   procedure Run (Self : in out Test_Case) is abstract;
   --  Execute test case

   function Status (Self : Test_Case) return Status_Kind is abstract;
   --  Execution result

   function Duration (Self : Test_Case) return League.Calendars.Time
      is abstract;
   --  Duration of test's run

   function Name (Self : Test_Case) return League.Strings.Universal_String
      is abstract;
   --  Name of the test case

   function Fixture (Self : Test_Case) return League.Strings.Universal_String
      is abstract;
   --  Name of test fixture

   function File (Self : Test_Case) return League.Strings.Universal_String
      is abstract;
   --  Path to test file inside svn tree

   function Output (Self : Test_Case) return League.Strings.Universal_String
      is abstract;
   --  The output from the test case

   function Traceback (Self : Test_Case) return League.Strings.Universal_String
      is abstract;
   --  The traceback from any error or failure

   Ok : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("OK" & Wide_Wide_Character'Val (10));

end Gela.Test_Cases;
