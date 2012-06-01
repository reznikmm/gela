------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Input_Test_Cases is

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

   ---------------
   -- GPR_Build --
   ---------------

   function GPR_Build
     (Self : Test_Case)
      return Gela.Build_Test_Cases.Test_Case_Access is
   begin
      return Self.Run_Test.GPR_Build;
   end GPR_Build;

   ------------
   -- Create --
   ------------

   function Create
     (Run_Test  : Gela.Run_Test_Cases.Test_Case_Access;
      Input     : League.Strings.Universal_String)
      return Run_Test_Cases.Test_Case'Class
   is
      Arguments : League.String_Vectors.Universal_String_Vector :=
        Run_Test.Arguments;
      Output    : League.Strings.Universal_String;
      Result    : constant Test_Case :=
        (Run_Test_Cases.Test_Case with
         Run_Test  => Run_Test,
         Input     => Input);
   begin
      Arguments.Append (Input);
      Output := Run_Test.Path & "/" & Input;
      Output.Replace (Output.Length - 2, Output.Length, ".out");

      Run_Test.Set_Output_Name (Output);

      Run_Test.Set_Command
        (Command  => Run_Test.Command,
         Arguments => Arguments);
      return Result;
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

   ---------------------
   -- Set_Output_Name --
   ---------------------

   procedure Set_Output_Name
     (Self  : in out Test_Case;
      Value : League.Strings.Universal_String) is
   begin
      Self.Run_Test.Set_Output_Name (Value);
   end Set_Output_Name;

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

end Gela.Input_Test_Cases;
