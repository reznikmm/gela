------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Test_Cases.Input is

   ---------------
   -- Arguments --
   ---------------

   function Arguments
     (Self : Test_Case)
      return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Run_Test.Arguments;
   end Arguments;

   -----------
   -- Build --
   -----------

   function Build (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Run_Test.Build;
   end Build;

   -------------
   -- Command --
   -------------

   function Command (Self : Test_Case) return Universal_String is
   begin
      return Self.Run_Test.Command;
   end Command;

   ------------
   -- Create --
   ------------

   function Create
     (Directory : Ada.Directories.Directory_Entry_Type;
      Build     : League.Strings.Universal_String;
      Input     : League.Strings.Universal_String;
      Expect    : League.Strings.Universal_String := Ok)
      return Test_Cases.Execute.Test_Case_Access
   is
      Result    : constant Test_Case :=
        (Test_Cases.Execute.Test_Case with
           Run_Test  => Gela.Test_Cases.Execute.Create
             (Directory, Build, Expect),
         Input     => Input);
      Arguments : League.String_Vectors.Universal_String_Vector :=
        Result.Run_Test.Arguments;
      Output    : League.Strings.Universal_String;
   begin
      Arguments.Append (Input);
      Output := Result.Run_Test.Path & "/" & Input;
      Output.Replace (Output.Length - 2, Output.Length, ".out");

      Result.Run_Test.Set_Command
        (Command   => Result.Run_Test.Command,
         Arguments => Arguments);
      return new Test_Case'(Result);
   end Create;

   --------------
   -- Duration --
   --------------

   function Duration (Self : Test_Case) return League.Calendars.Time is
   begin
      return Self.Run_Test.Duration;
   end Duration;

   ----------
   -- File --
   ----------

   function File (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Run_Test.File;
   end File;

   -------------
   -- Fixture --
   -------------

   function Fixture
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Run_Test.Fixture;
   end Fixture;

   ----------
   -- Name --
   ----------

   function Name (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Run_Test.Name & " " & Self.Input;
   end Name;

   ------------
   -- Output --
   ------------

   function Output
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Run_Test.Output;
   end Output;

   ----------
   -- Path --
   ----------

   function Path (Self : Test_Case) return Universal_String is
   begin
      return Self.Run_Test.Path;
   end Path;

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Test_Case) is
   begin
      Self.Run_Test.Run;
   end Run;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
     (Self      : in out Test_Case;
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector) is
   begin
      Self.Run_Test.Set_Command (Command, Arguments);
   end Set_Command;

   procedure Set_Name
     (Self  : in out Test_Case;
      Value : League.Strings.Universal_String) is
   begin
      Self.Run_Test.Set_Name (Value);
   end Set_Name;

   ------------
   -- Status --
   ------------

   function Status (Self : Test_Case) return Test_Cases.Status_Kind is
   begin
      return Self.Run_Test.Status;
   end Status;

   ---------------
   -- Traceback --
   ---------------

   function Traceback
     (Self : Test_Case)
      return League.Strings.Universal_String
   is
   begin
      return Self.Run_Test.Traceback;
   end Traceback;

end Gela.Test_Cases.Input;
