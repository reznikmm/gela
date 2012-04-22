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

package Gela.Ada_Test_Cases is

   type Test_Case is new Test_Cases.Test_Case with private;

   procedure Run (Self : in out Test_Case);
   --  Compile source into executable:
   --         gprbuild -p -aP ../source/ -aP <TEST>/.. \
   --             -XGELA_LIB_DIR=$(BUILD)/gela \
   --             -XSOURCE_DIR=<TEST> \
   --             -XOBJECT_DIR=$(BUILD)/<TEST> \
   --             -P simple.gpr main.adb
   --  Run executable:
   --         cd <TEST>; $(TEST_HOME)/$</main > $(TEST_HOME)/$@
   --  Compare output with <TEST>.out or "OK" if no such file

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
      Build     : League.Strings.Universal_String)
     return Test_Cases.Test_Case'Class;
   --  Directory point where test locates.
   --  Build point to directory where tests will be build.

private

   use League.Strings;

   type Test_Case is new Test_Cases.Test_Case with record
      Build     : League.Strings.Universal_String;
      Full_Path : League.Strings.Universal_String;
      Name      : League.Strings.Universal_String;
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

   function XGELA_LIB_DIR (Self : Test_Case) return Universal_String;
   function XSOURCE_DIR (Self : Test_Case) return Universal_String;
   function XOBJECT_DIR (Self : Test_Case) return Universal_String;

end Gela.Ada_Test_Cases;
