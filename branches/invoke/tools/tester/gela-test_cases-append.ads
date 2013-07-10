------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Calendars;
with League.String_Vectors;
with League.Strings;
with Gela.Test_Cases.Execute;

package Gela.Test_Cases.Append is

   function "+" (Left, Right : access Test_Cases.Test_Case'Class)
     return Gela.Test_Cases.Test_Case_Access;

   function "+"
     (Left : access Test_Cases.Test_Case'Class;
      Right : access Gela.Test_Cases.Execute.Test_Case'Class)
     return Gela.Test_Cases.Execute.Test_Case_Access;

private

   type Test_Cases_Array is array (1 .. 2) of Test_Case_Access;

   type Test_Case is new Test_Cases.Test_Case with record
      List     : Test_Cases_Array;
      Last     : Natural := 0;
      Duration : League.Calendars.Time;
   end record;

   procedure Run (Self : in out Test_Case);

   function Status (Self : Test_Case) return Status_Kind;

   function Duration (Self : Test_Case) return League.Calendars.Time;

   function Name (Self : Test_Case) return League.Strings.Universal_String;

   function Fixture (Self : Test_Case) return League.Strings.Universal_String;

   function File (Self : Test_Case) return League.Strings.Universal_String;

   function Output (Self : Test_Case) return League.Strings.Universal_String;

   function Traceback
     (Self : Test_Case)
      return League.Strings.Universal_String;

   type Run_Test_Case is new Test_Cases.Execute.Test_Case with record
      On_Left  : Boolean;
      Left     : Test_Case_Access;
      Right    : Gela.Test_Cases.Execute.Test_Case_Access;
      Duration : League.Calendars.Time;
   end record;

   procedure Run (Self : in out Run_Test_Case);

   function Status (Self : Run_Test_Case) return Status_Kind;

   function Duration (Self : Run_Test_Case) return League.Calendars.Time;

   function Name (Self : Run_Test_Case) return League.Strings.Universal_String;

   function Fixture
     (Self : Run_Test_Case)
      return League.Strings.Universal_String;

   function File (Self : Run_Test_Case) return League.Strings.Universal_String;

   function Output
     (Self : Run_Test_Case)
      return League.Strings.Universal_String;

   function Traceback
     (Self : Run_Test_Case)
      return League.Strings.Universal_String;

   function Command
     (Self : Run_Test_Case)
      return League.Strings.Universal_String;

   function Arguments
     (Self : Run_Test_Case)
      return League.String_Vectors.Universal_String_Vector;

   procedure Set_Command
     (Self      : in out Run_Test_Case;
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector);

   procedure Set_Name
     (Self  : in out Run_Test_Case;
      Value : League.Strings.Universal_String);

   function Build
     (Self : Run_Test_Case)
      return League.Strings.Universal_String;

   function Path
     (Self : Run_Test_Case)
      return League.Strings.Universal_String;

end Gela.Test_Cases.Append;
