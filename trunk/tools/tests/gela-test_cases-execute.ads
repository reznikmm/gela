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
with League.Strings;
with League.String_Vectors;

package Gela.Test_Cases.Execute is

   type Test_Case is  abstract new Test_Cases.Test_Case with null record;
   type Test_Case_Access is access all Test_Case'Class;

   procedure Run (Self : in out Test_Case) is abstract;
   --  1. Run executable:
   --         cd <TEST>; $(TEST_HOME)/$</main > $(TEST_HOME)/$@
   --
   --  2. Compare output with <TEST>.out or "OK" if no such file

   function Create
     (Directory : Ada.Directories.Directory_Entry_Type;
      Build     : League.Strings.Universal_String;
      Expect    : League.Strings.Universal_String := Ok)
     return Test_Case_Access;
   --  Directory point where test locates.
   --  Build point to directory where tests will be build.
   --  Input - first argument of test

   --  Customisation interface

   function Build
     (Self : Test_Case)
      return League.Strings.Universal_String is abstract;
   --  Build point to directory where tests will be build

   function Path
     (Self : Test_Case)
      return League.Strings.Universal_String is abstract;
   --  Directory where test is located

   function Command
     (Self : Test_Case)
      return League.Strings.Universal_String is abstract;
   --  Command to run test

   function Arguments
     (Self : Test_Case) return League.String_Vectors.Universal_String_Vector
      is abstract;
   --  Arguments for command to run test

   procedure Set_Command
     (Self      : in out Test_Case;
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector) is abstract;

   procedure Set_Name
     (Self  : in out Test_Case;
      Value : League.Strings.Universal_String) is abstract;
   --  Change test's name

end Gela.Test_Cases.Execute;
