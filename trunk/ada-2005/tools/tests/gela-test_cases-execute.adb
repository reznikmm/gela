------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Host;
with League.Calendars;

package body Gela.Test_Cases.Execute is

   package Concrete is
      use League.Strings;

      type Test_Case is new Test_Cases.Execute.Test_Case with record
         Command   : League.Strings.Universal_String;
         Arguments : League.String_Vectors.Universal_String_Vector;
         Expect    : League.Strings.Universal_String;
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

      procedure Run (Self : in out Test_Case);
      --  Run executable:
      --   (if no Input)
      --         cd <TEST>; $(TEST_HOME)/$</main > $(TEST_HOME)/$@
      --   (if has Input)
      --         cd <TEST>; $(TEST_HOME)/$</main Input > $(TEST_HOME)/Input
      --  Compare output with <TEST>.out, <Input>.out or "OK" if no such file

      function Status    (Self : Test_Case) return Test_Cases.Status_Kind;
      function Duration  (Self : Test_Case) return League.Calendars.Time;
      function Name      (Self : Test_Case) return Universal_String;
      function Fixture   (Self : Test_Case) return Universal_String;
      function File      (Self : Test_Case) return Universal_String;
      function Output    (Self : Test_Case) return Universal_String;
      function Traceback (Self : Test_Case) return Universal_String;

      function Build (Self : Test_Case) return Universal_String;

      function Path (Self : Test_Case) return League.Strings.Universal_String;
      --  Directory where test is located

      function Command
        (Self : Test_Case)
      return League.Strings.Universal_String;
      --  Command to run test

      function Arguments
        (Self : Test_Case)
      return League.String_Vectors.Universal_String_Vector;
      --  Arguments for command to run test

      procedure Set_Command
        (Self      : in out Test_Case;
         Command   : League.Strings.Universal_String;
         Arguments : League.String_Vectors.Universal_String_Vector);

      procedure Set_Name
        (Self  : in out Test_Case;
         Value : League.Strings.Universal_String);

   end Concrete;

   package body Concrete is

      ---------------
      -- Arguments --
      ---------------

      function Arguments
        (Self : Test_Case)
         return League.String_Vectors.Universal_String_Vector is
      begin
         return Self.Arguments;
      end Arguments;

      -----------
      -- Build --
      -----------

      function Build (Self : Test_Case) return Universal_String is
      begin
         return Self.Build;
      end Build;

      -------------
      -- Command --
      -------------

      function Command
        (Self : Test_Case)
      return League.Strings.Universal_String is
      begin
         return Self.Command;
      end Command;

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

      function File (Self : Test_Case) return Universal_String is
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

      function Name (Self : Test_Case) return Universal_String is
      begin
         return Self.Name;
      end Name;

      ----------------
      -- Object_Dir --
      ----------------

      function Object_Dir (Self : Test_Case) return Universal_String is
         File : constant String := Self.Full_Path.To_UTF_8_String;
         Name : constant String := Ada.Directories.Simple_Name (File);
         Obj  : constant String := Ada.Directories.Compose
           (Self.Build.To_UTF_8_String, Name);
      begin
         return League.Strings.From_UTF_8_String (Obj);
      end Object_Dir;

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

      -----------------
      -- Output_File --
      -----------------

      function Output_File (Self : Test_Case) return Universal_String is
      begin
         return Self.Name & ".log";
      end Output_File;

      ------------
      -- Parent --
      ------------

      function Parent (Self : Test_Case) return Universal_String is
         Tests : constant String := Ada.Directories.Containing_Directory
           (Self.Full_Path.To_UTF_8_String);
      begin
         return League.Strings.From_UTF_8_String (Tests);
      end Parent;

      ----------
      -- Path --
      ----------

      function Path (Self : Test_Case) return Universal_String is
      begin
         return Self.Full_Path;
      end Path;

      ---------
      -- Run --
      ---------

      procedure Run (Self : in out Test_Case) is
         procedure Run_Test;
         procedure Check_Output;

         function Code_To_Status
           (Code : Integer)
            return Gela.Test_Cases.Status_Kind;

         function Code_To_Status
           (Code : Integer)
            return Gela.Test_Cases.Status_Kind is
         begin
            if Code = 0 then
               return Test_Cases.Success;
            else
               return Test_Cases.Failure;
            end if;
         end Code_To_Status;

         procedure Check_Output is
         begin
            if Self.Expect /= Self.Output then
               Self.Status := Test_Cases.Failure;
            end if;
         end Check_Output;

         procedure Run_Test is
            Code : Integer;
         begin
            Host.Execute
              (Command     => Self.Command,
               Arguments   => Self.Arguments,
               Exit_Code   => Code,
               Output      => Self.Output,
               Output_File => Self.Output_File,
               Directory   => Self.Full_Path);

            Self.Status := Code_To_Status (Code);
         end Run_Test;

         use type League.Calendars.Date_Time;
         use type Test_Cases.Status_Kind;

         Started : constant League.Calendars.Date_Time :=
           League.Calendars.Clock;
      begin
         Run_Test;

         if Self.Status = Test_Cases.Success then
            Check_Output;
         else
            Self.Traceback := Self.Output;
         end if;

         Self.Duration := League.Calendars.Clock - Started;
      end Run;

      -----------------
      -- Set_Command --
      -----------------

      procedure Set_Command
        (Self      : in out Test_Case;
         Command   : League.Strings.Universal_String;
         Arguments : League.String_Vectors.Universal_String_Vector) is
      begin
         Self.Command := Command;
         Self.Arguments := Arguments;
      end Set_Command;

      --------------
      -- Set_Name --
      --------------

      procedure Set_Name
        (Self  : in out Test_Case;
         Value : League.Strings.Universal_String) is
      begin
         Self.Name := Value;
      end Set_Name;

      ------------
      -- Source --
      ------------

      function Source (Self : Test_Case) return Universal_String is
         use Ada.Directories;
         Test   : constant String := Self.Full_Path.To_UTF_8_String;
         Parent : constant String := Containing_Directory (Test);
         Tests  : constant String := Containing_Directory (Parent);
         Trunk  : constant String := Containing_Directory (Tests);
      begin
         return League.Strings.From_UTF_8_String (Trunk);
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

   end Concrete;

   ------------
   -- Create --
   ------------

   function Create
     (Directory : Ada.Directories.Directory_Entry_Type;
      Build     : League.Strings.Universal_String;
      Expect    : League.Strings.Universal_String := Ok)
      return Test_Case_Access
   is
      use Ada.Directories;
      use type League.Strings.Universal_String;

      Name : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_8_String (Simple_Name (Directory));

      Result : Concrete.Test_Case :=
        (Build     => Build,
         Expect    => Expect,
         Full_Path => League.Strings.From_UTF_8_String (Full_Name (Directory)),
         Name      => Name,
         Status    => Test_Cases.Error,
         Fixture   => League.Strings.To_Universal_String ("Ada_Test_Cases"),
         File      => "tests/" & Name,
         others    => <>);

   begin
      Result.Command := Result.Object_Dir & "/main";

      return new Concrete.Test_Case'(Result);
   end Create;

end Gela.Test_Cases.Execute;
