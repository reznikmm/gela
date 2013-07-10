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
with League.String_Vectors;
with Gela.Test_Cases;

package Gela.Test_Cases.Build is

   type Test_Case is new Test_Cases.Test_Case with private;
   type Test_Case_Access is access all Test_Case'Class;

   overriding
   procedure Run (Self : in out Test_Case);
   --  Compile source using gprbuild:
   --         gprbuild -j0 -p -aP <SOURCE>/gnat/ \
   --             -aP <SEARCH_PATH> \
   --             -XGELA_BUILD=<BUILD> \
   --             -P <PROJECT> <OPTIONS>

   function Create
     (Source    : League.Strings.Universal_String;
      Build     : League.Strings.Universal_String;
      Project   : League.Strings.Universal_String;
      Search    : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector;
      Options   : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector;
      Name      : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
     return Test_Case;

   --  Customisation interface

   function Search
     (Self : Test_Case) return League.String_Vectors.Universal_String_Vector;

   procedure Set_Search
     (Self  : in out Test_Case;
      Value : League.String_Vectors.Universal_String_Vector);

   function Options
     (Self : Test_Case) return League.String_Vectors.Universal_String_Vector;

   procedure Set_Options
     (Self  : in out Test_Case;
      Value : League.String_Vectors.Universal_String_Vector);

private

   use League.Strings;

   type Test_Case is new Test_Cases.Test_Case with record
      Source    : League.Strings.Universal_String;
      Build     : League.Strings.Universal_String;
      Project   : League.Strings.Universal_String;
      Search    : League.String_Vectors.Universal_String_Vector;
      Options   : League.String_Vectors.Universal_String_Vector;
      Name      : League.Strings.Universal_String;
      Status    : Test_Cases.Status_Kind;
      Duration  : League.Calendars.Time;
      Fixture   : League.Strings.Universal_String;
      Output    : League.Strings.Universal_String;
      Traceback : League.Strings.Universal_String;
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

   function Output_File (Self : Test_Case) return Universal_String;
   --  Where to save test's output (<TEST>.log)

end Gela.Test_Cases.Build;
