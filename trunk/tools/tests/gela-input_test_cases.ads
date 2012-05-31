------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases;
with League.Calendars;
with League.Strings;

with Gela.Run_Test_Cases;
with Gela.Build_Test_Cases;
with League.String_Vectors;

package Gela.Input_Test_Cases is

   type Test_Case is new Run_Test_Cases.Test_Case with private;

   procedure Run (Self : in out Test_Case);
   --  Run executable with Input argument:
   --     cd <TEST>; $(TEST_HOME)/$</main Input > $(TEST_HOME)/Input
   --  Compare output with <Input>.out or "OK" if no such file

   function Create
     (Run_Test  : Gela.Run_Test_Cases.Test_Case_Access;
      Input     : League.Strings.Universal_String)
     return Run_Test_Cases.Test_Case'Class;
   --  Run_Test - base test.
   --  Input - first argument of test

private

   use League.Strings;

   type Test_Case is new Run_Test_Cases.Test_Case with record
      Run_Test  : Gela.Run_Test_Cases.Test_Case_Access;
      Input     : League.Strings.Universal_String;
   end record;

   overriding
   function Status (Self : Test_Case) return Test_Cases.Status_Kind;

   overriding
   function Duration (Self : Test_Case) return League.Calendars.Time;

   overriding
   function Name (Self : Test_Case) return Universal_String;

   overriding
   function Fixture (Self : Test_Case) return Universal_String;

   overriding
   function File (Self : Test_Case) return Universal_String;

   overriding
   function Output (Self : Test_Case) return Universal_String;

   overriding
   function Traceback (Self : Test_Case) return Universal_String;

   overriding
   function GPR_Build
     (Self : Test_Case)
      return Gela.Build_Test_Cases.Test_Case_Access;

   overriding
   function Path (Self : Test_Case) return Universal_String;

   overriding
   function Command (Self : Test_Case) return Universal_String;

   overriding
   function Arguments
     (Self : Test_Case)
      return League.String_Vectors.Universal_String_Vector;
   --  Arguments for command to run test

   overriding
   procedure Set_Command
     (Self      : in out Test_Case;
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector);

   overriding
   procedure Set_Output_Name
     (Self  : in out Test_Case;
      Value : League.Strings.Universal_String);

end Gela.Input_Test_Cases;
