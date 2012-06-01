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
with Gela.Test_Cases;
with Gela.Run_Test_Cases;

package Gela.Valgrind_Test_Cases is

   type Test_Case is new Test_Cases.Test_Case with private;
   type Test_Case_Access is access all Test_Case'Class;

   overriding
   procedure Run (Self : in out Test_Case);
   --  Run executable with valgrind:
   --     valgrind cmd args

   function Create
     (Run_Test  : Gela.Run_Test_Cases.Test_Case_Access)
     return Test_Case;

private

   use League.Strings;

   type Test_Case is new Test_Cases.Test_Case with record
      Output    : League.Strings.Universal_String;
      Errors    : Boolean;
      Run_Test  : Gela.Run_Test_Cases.Test_Case_Access;
   end record;

   overriding
   function Status (Self : Test_Case) return Test_Cases.Status_Kind;

   overriding
   function Duration (Self : Test_Case) return League.Calendars.Time;

   overriding
   function Name (Self : Test_Case) return League.Strings.Universal_String;
   --  Return Project if Name not set

   overriding
   function Fixture (Self : Test_Case) return League.Strings.Universal_String;

   overriding
   function File (Self : Test_Case) return League.Strings.Universal_String;
   --  Return Project

   overriding
   function Output (Self : Test_Case) return League.Strings.Universal_String;

   overriding
   function Traceback
     (Self : Test_Case) return League.Strings.Universal_String;

   not overriding
   function Valgrind_Report
     (Self : Test_Case) return League.Strings.Universal_String;

   not overriding procedure Read_Valgrind_Report (Self : in out Test_Case);

end Gela.Valgrind_Test_Cases;
