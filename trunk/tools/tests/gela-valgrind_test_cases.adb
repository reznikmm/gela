------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.String_Vectors;

package body Gela.Valgrind_Test_Cases is

   ------------
   -- Create --
   ------------

   function Create
     (Run_Test  : Gela.Run_Test_Cases.Test_Case_Access)
      return Test_Case
   is
      Prefix    : League.String_Vectors.Universal_String_Vector;
      Arguments : League.String_Vectors.Universal_String_Vector :=
        Run_Test.Arguments;
      Result    : constant Test_Case :=
        (Run_Test  => Run_Test);
   begin
      Prefix.Append (To_Universal_String ("--xml=yes"));
      Prefix.Append ("--xml-file=" & Run_Test.Path & Result.Valgrind_Report);
      Prefix.Append (Run_Test.Command);
      Arguments.Prepend (Prefix);
      Run_Test.Set_Command
        (Command   => To_Universal_String ("valgrind"),
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
      return "valgrind " & Self.Run_Test.Name;
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

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Test_Case) is
   begin
      Self.Run_Test.Run;
   end Run;

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

   ---------------------
   -- Valgrind_Report --
   ---------------------

   not overriding
   function Valgrind_Report
     (Self : Test_Case) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return To_Universal_String ("/tmp/valgrind.xml");
   end Valgrind_Report;
end Gela.Valgrind_Test_Cases;
