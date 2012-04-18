------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Conv;
with Gela.Host;
with League.Application;
with League.String_Vectors;

package body Gela.Ada_Test_Cases is

   Test_Home : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("TEST_HOME");

   Test_Home_Value : constant League.Strings.Universal_String :=
     League.Application.Environment.Value (Test_Home);

   ------------
   -- Create --
   ------------

   function Create
     (Directory : Ada.Directories.Directory_Entry_Type)
     return Test_Cases.Test_Case'Class
   is
      use Ada.Directories;
      Result : constant Test_Case :=
        (Name    => Conv.To_Universal_String (Simple_Name (Directory)),
         Status  => Test_Cases.Error,
         Fixture => League.Strings.To_Universal_String ("Ada_Test_Cases"),
         File    => Conv.To_Universal_String (Full_Name (Directory)),
         others  => <>);
   begin
      return Result;
   end Create;

   --------------
   -- Duration --
   --------------

   function Duration (Self : Test_Case) return League.Calendars.Time is
   begin
      return Self.Duration;
   end Duration;

   ----------
   -- File --
   ----------

   function File (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.File;
   end File;

   -------------
   -- Fixture --
   -------------

   function Fixture
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Fixture;
   end Fixture;

   ----------
   -- Name --
   ----------

   function Name (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Name;
   end Name;

   ------------
   -- Output --
   ------------

   function Output
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Output;
   end Output;

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Test_Case) is
      procedure Run_Gprbuild;

      procedure Run_Gprbuild is

         function "+"
           (Text : Wide_Wide_String)
           return League.Strings.Universal_String
           renames League.Strings.To_Universal_String;

         Gprbuild  : constant League.Strings.Universal_String :=
           +"gprbuild";

         Arguments : League.String_Vectors.Universal_String_Vector;

         Code   : Integer;
      begin
         Arguments.Append (+"-p");
         Arguments.Append (+"-aP");
         Arguments.Append (Self.Source);
         Arguments.Append (Self.XGELA_LIB_DIR);
         Arguments.Append (Self.XSOURCE_DIR);
         Arguments.Append (Self.XOBJECT_DIR);
         Arguments.Append (+"-P");
         Arguments.Append (+"simple.gpr");
         Arguments.Append (+"main.adb");

         Host.Execute
           (Gprbuild, Arguments, Code, Self.Output);

         if Code = 0 then
            Self.Status := Test_Cases.Success;
         else
            Self.Status := Test_Cases.Failure;
         end if;

      end Run_Gprbuild;

      use type League.Calendars.Date_Time;

--      Started : constant League.Calendars.Date_Time :=
--        League.Calendars.Clock;
   begin
      Run_Gprbuild;

--      Self.Duration := League.Calendars.Clock - Started;
   end Run;

   ------------
   -- Source --
   ------------

   function Source (Self : Test_Case) return Universal_String is
      Tests : constant String := Ada.Directories.Containing_Directory
        (Conv.To_String (Self.File));
      Source : constant String := Ada.Directories.Containing_Directory (Tests);
   begin
      return Conv.To_Universal_String (Source);
   end Source;

   ------------
   -- Status --
   ------------

   function Status (Self : Test_Case) return Test_Cases.Status_Kind is
   begin
      return Self.Status;
   end Status;

   ---------------
   -- Traceback --
   ---------------

   function Traceback
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Traceback;
   end Traceback;

   -------------------
   -- XGELA_LIB_DIR --
   -------------------

   function XGELA_LIB_DIR (Self : Test_Case) return Universal_String is
      pragma Unreferenced (Self);
      Gela : constant String := Ada.Directories.Compose
        (Conv.To_String (Test_Home_Value), "gela");
   begin
      return "-XGELA_LIB_DIR=" & Conv.To_Universal_String (Gela);
   end XGELA_LIB_DIR;

   -----------------
   -- XSOURCE_DIR --
   -----------------

   function XSOURCE_DIR (Self : Test_Case) return Universal_String is
   begin
      return "-XSOURCE_DIR=" & Self.File;
   end XSOURCE_DIR;

   -----------------
   -- XOBJECT_DIR --
   -----------------

   function XOBJECT_DIR (Self : Test_Case) return Universal_String is
      File : constant String := Conv.To_String (Self.File);
      Name : constant String := Ada.Directories.Simple_Name (File);
      Obj  : constant String := Ada.Directories.Compose
        (Conv.To_String (Test_Home_Value), Name);
   begin
      return "-XOBJECT_DIR=" & Conv.To_Universal_String (Obj);
   end XOBJECT_DIR;

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Self : Test_Case) return Universal_String is
      pragma Unreferenced (Self);
   begin
      return Empty_Universal_String;
   end Output_File;

end Gela.Ada_Test_Cases;
