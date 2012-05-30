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
     (Directory : Ada.Directories.Directory_Entry_Type;
      Build     : League.Strings.Universal_String;
      Input     : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
     return Test_Cases.Test_Case'Class
   is
      use Ada.Directories;

      Name : constant League.Strings.Universal_String :=
        Conv.To_Universal_String (Simple_Name (Directory));

      Result : Test_Case :=
        (Build     => Build,
         Full_Path => Conv.To_Universal_String (Full_Name (Directory)),
         Name      => Name,
         Input     => Input,
         Status    => Test_Cases.Error,
         Fixture   => League.Strings.To_Universal_String ("Ada_Test_Cases"),
         File      => (+"tests/") & Name,
         others    => <>);

      Options : League.String_Vectors.Universal_String_Vector;
   begin
      Options.Append (+"main.adb");
      Options.Append ("-XSOURCE_DIR=" & Name);
      Options.Append ("-XOBJECT_DIR=" & Result.Object_Dir);

      Result.Gprbuild := new Gela.Build_Test_Cases.Test_Case'
        (Gela.Build_Test_Cases.Create
           (Source  => Result.Source,
            Build   => Build,
            Project => Result.Parent & "/simple.gpr",
            Options => Options));
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
      if Self.Input.Is_Empty then
         return Self.Name;
      else
         return Self.Name & " " & Self.Input;
      end if;
   end Name;

   ----------------
   -- Object_Dir --
   ----------------

   function Object_Dir (Self : Test_Case) return Universal_String is
      File : constant String := Conv.To_String (Self.Full_Path);
      Name : constant String := Ada.Directories.Simple_Name (File);
      Obj  : constant String := Ada.Directories.Compose
        (Conv.To_String (Self.Build), Name);
   begin
      return Conv.To_Universal_String (Obj);
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

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Test_Case) is
      procedure Run_Gprbuild;
      procedure Run_Test;
      procedure Check_Output;
      function Code_To_Status (Code : Integer) return Test_Cases.Status_Kind;

      function Code_To_Status (Code : Integer) return Test_Cases.Status_Kind is
      begin
         if Code = 0 then
            return Test_Cases.Success;
         else
            return Test_Cases.Failure;
         end if;
      end Code_To_Status;

      procedure Check_Output is
         Per_Input  : League.Strings.Universal_String;
         Out_File   : constant League.Strings.Universal_String :=
           Self.Full_Path & "/" & Self.Name & ".out";
         Expect : League.Strings.Universal_String;
      begin
         if not Self.Input.Is_Empty then
            Per_Input := Self.Full_Path & "/" & Self.Input;
            Per_Input.Replace (Per_Input.Length - 2, Per_Input.Length, ".out");
         end if;

         if Ada.Directories.Exists (Conv.To_String (Per_Input)) then
            Expect := Conv.Read_File (Per_Input);
         elsif Ada.Directories.Exists (Conv.To_String (Out_File)) then
            Expect := Conv.Read_File (Out_File);
         else
            Expect := +"OK";
            Expect.Append (Wide_Wide_Character'Val (10));
         end if;

         if Expect /= Self.Output then
            Self.Status := Test_Cases.Failure;
         end if;
      end Check_Output;

      procedure Run_Gprbuild is
      begin
         Self.Gprbuild.Run;
         Self.Status := Self.Gprbuild.Status;
         Self.Duration := Self.Gprbuild.Duration;
         Self.Output := Self.Gprbuild.Output;
      end Run_Gprbuild;

      procedure Run_Test is
         Code      : Integer;
         Arguments : League.String_Vectors.Universal_String_Vector;
      begin
         if not Self.Input.Is_Empty then
            Arguments.Append (Self.Input);
         end if;

         Host.Execute
           (Command     => Self.Object_Dir & "/main",
            Arguments   => Arguments,
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
      Run_Gprbuild;

      if Self.Status = Test_Cases.Success then
         Run_Test;
      end if;

      if Self.Status = Test_Cases.Success then
         Check_Output;
      end if;

      Self.Duration := League.Calendars.Clock - Started;
   end Run;

   ------------
   -- Source --
   ------------

   function Source (Self : Test_Case) return Universal_String is
      Test : constant String := Conv.To_String (Self.Full_Path);
      Parent : constant String := Ada.Directories.Containing_Directory (Test);
      Tests : constant String := Ada.Directories.Containing_Directory (Parent);
      Trunk : constant String := Ada.Directories.Containing_Directory (Tests);
   begin
      return Conv.To_Universal_String (Trunk);
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

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Self : Test_Case) return Universal_String is
      Result : Universal_String := Self.Name;
   begin
      if not Self.Input.Is_Empty then
         Result.Append ('-');
         Result.Append (Self.Input);
      end if;

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
