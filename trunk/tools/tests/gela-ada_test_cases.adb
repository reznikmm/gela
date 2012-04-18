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
--  with League.Application;
with League.String_Vectors;

package body Gela.Ada_Test_Cases is

   function "+"
     (Text : Wide_Wide_String)
     return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   ------------
   -- Create --
   ------------

   function Create
     (Test_Home : League.Strings.Universal_String;
      Directory : Ada.Directories.Directory_Entry_Type)
     return Test_Cases.Test_Case'Class
   is
      use Ada.Directories;

      Name : constant League.Strings.Universal_String :=
        Conv.To_Universal_String (Simple_Name (Directory));

      Result : constant Test_Case :=
        (Test_Home => Test_Home,
         Full_Path => Conv.To_Universal_String (Full_Name (Directory)),
         Name      => Name,
         Status    => Test_Cases.Error,
         Fixture   => League.Strings.To_Universal_String ("Ada_Test_Cases"),
         File      => (+"tests/") & Name,
         others    => <>);
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

         Gprbuild  : constant League.Strings.Universal_String :=
           +"gprbuild";

         Arguments : League.String_Vectors.Universal_String_Vector;

         Code   : Integer;
      begin
         Arguments.Append (+"-p");
         Arguments.Append (+"-aP");
         Arguments.Append (Self.Source);
         Arguments.Append (+"-aP");
         Arguments.Append (Self.Parent);
         Arguments.Append (Self.XGELA_LIB_DIR);
         Arguments.Append (Self.XSOURCE_DIR);
         Arguments.Append (Self.XOBJECT_DIR);
         Arguments.Append (+"-P");
         Arguments.Append (+"simple.gpr");
         Arguments.Append (+"main.adb");

         Host.Execute
           (Gprbuild, Arguments, Code, Self.Output, Self.Output_File);

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
        (Conv.To_String (Self.Full_Path));
      Parent : constant String := Ada.Directories.Containing_Directory (Tests);
      Source : constant String := Ada.Directories.Compose (Parent, "source");
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
      Gela : constant String := Ada.Directories.Compose
        (Conv.To_String (Self.Test_Home), "gela");
   begin
      return "-XGELA_LIB_DIR=" & Conv.To_Universal_String (Gela);
   end XGELA_LIB_DIR;

   -----------------
   -- XSOURCE_DIR --
   -----------------

   function XSOURCE_DIR (Self : Test_Case) return Universal_String is
   begin
      return "-XSOURCE_DIR=" & Self.Full_Path;
   end XSOURCE_DIR;

   -----------------
   -- XOBJECT_DIR --
   -----------------

   function XOBJECT_DIR (Self : Test_Case) return Universal_String is
      File : constant String := Conv.To_String (Self.Full_Path);
      Name : constant String := Ada.Directories.Simple_Name (File);
      Obj  : constant String := Ada.Directories.Compose
        (Conv.To_String (Self.Test_Home), Name);
   begin
      return "-XOBJECT_DIR=" & Conv.To_Universal_String (Obj);
   end XOBJECT_DIR;

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Self : Test_Case) return Universal_String is
      Result : Universal_String := Self.Name;
   begin
      Result.Append (".log");

      return Result;
   end Output_File;

   function Parent (Self : Test_Case) return Universal_String is
      Tests : constant String := Ada.Directories.Containing_Directory
        (Conv.To_String (Self.Full_Path));
   begin
      return Conv.To_Universal_String (Tests);
   end Parent;

end Gela.Ada_Test_Cases;
