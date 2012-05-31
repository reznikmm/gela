------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Directories;
with Gela.Test_Cases;
with League.Calendars;
with League.Strings;
with League.String_Vectors;

with Gela.Build_Test_Cases;

package Gela.Run_Test_Cases is

   type Test_Case is new Test_Cases.Test_Case with private;

   procedure Run (Self : in out Test_Case);
   --  Compile source into executable:
   --         gprbuild -p -aP ../../../gnat/ -aP <TEST>/.. \
   --             -XGELA_BUILD=$(BUILD)/gela \
   --             -XSOURCE_DIR=<TEST> \
   --             -XOBJECT_DIR=$(BUILD)/<TEST> \
   --             -P simple.gpr main.adb
   --  Run executable:
   --   (if no Input)
   --         cd <TEST>; $(TEST_HOME)/$</main > $(TEST_HOME)/$@
   --   (if has Input)
   --         cd <TEST>; $(TEST_HOME)/$</main Input > $(TEST_HOME)/Input
   --  Compare output with <TEST>.out, <Input>.out or "OK" if no such file

   function Status (Self : Test_Case) return Test_Cases.Status_Kind;

   function Duration (Self : Test_Case) return League.Calendars.Time;

   function Name (Self : Test_Case) return League.Strings.Universal_String;

   function Fixture (Self : Test_Case) return League.Strings.Universal_String;

   function File (Self : Test_Case) return League.Strings.Universal_String;

   function Output (Self : Test_Case) return League.Strings.Universal_String;

   function Traceback
     (Self : Test_Case) return League.Strings.Universal_String;

   function Create
     (Directory : Ada.Directories.Directory_Entry_Type;
      Build     : League.Strings.Universal_String;
      Input     : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
     return Test_Cases.Test_Case'Class;
   --  Directory point where test locates.
   --  Build point to directory where tests will be build.
   --  Input - first argument of test

   --  Customisation interface

   function GPR_Build
     (Self : Test_Case)
      return Gela.Build_Test_Cases.Test_Case_Access;

   function Command
     (Self : Test_Case)
      return League.Strings.Universal_String;
   --  Command to run test

   function Arguments
     (Self : Test_Case) return League.String_Vectors.Universal_String_Vector;
   --  Arguments for command to run test

   procedure Set_Command
     (Self      : in out Test_Case;
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector);

   procedure Set_Output_Name
     (Self  : in out Test_Case;
      Value : League.Strings.Universal_String);

private

   use League.Strings;

   type Test_Case is new Test_Cases.Test_Case with record
      Gprbuild  : Gela.Build_Test_Cases.Test_Case_Access;
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Out_Name  : League.Strings.Universal_String;
      Build     : League.Strings.Universal_String;
      Full_Path : League.Strings.Universal_String;
      Name      : League.Strings.Universal_String;
      Input     : League.Strings.Universal_String;
      Status    : Test_Cases.Status_Kind;
      Duration  : League.Calendars.Time;
      Fixture   : League.Strings.Universal_String;
      File      : League.Strings.Universal_String;
      Output    : League.Strings.Universal_String;
      Traceback : League.Strings.Universal_String;
   end record;

   function Source (Self : Test_Case) return Universal_String;
   --  Path to Gela sources (trunk/source/)

   function Object_Dir (Self : Test_Case) return Universal_String;
   --  Path to test's object directory ($(BUILD)/<TEST>/)

   function Parent (Self : Test_Case) return Universal_String;
   --  Path to directory containing tests (truck/tests/)

   function Output_File (Self : Test_Case) return Universal_String;
   --  Where to save test's output (<TEST>.log)

end Gela.Run_Test_Cases;
