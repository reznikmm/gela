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
with Ada.Directories;

package body Gela.Build_Test_Cases is

   function "+"
     (Text : Wide_Wide_String)
     return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   ------------
   -- Create --
   ------------

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
     return Test_Case
   is
      use Ada.Directories;
   begin
      return
        (Source    => Source,
         Build     => Build,
         Project   => Project,
         Search    => Search,
         Options   => Options,
         Name      => Name,
         Status    => <>,
         Duration  => <>,
         Fixture   => +"gprbuild",
         Output    => <>,
         Traceback => <>);
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
      return Self.Project;
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
      if Self.Name.Is_Empty then
         return Self.Project;
      else
         return Self.Name;
      end if;
   end Name;

   -------------
   -- Options --
   -------------

   function Options
     (Self : Test_Case) return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Options;
   end Options;

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
      Log : constant String := Conv.To_String (Name (Self)) & ".log";
   begin
      return Conv.To_Universal_String (Ada.Directories.Simple_Name (Log));
   end Output_File;

   ---------
   -- Run --
   ---------

   procedure Run (Self : in out Test_Case) is
      function Code_To_Status (Code : Integer) return Test_Cases.Status_Kind;

      function Code_To_Status (Code : Integer) return Test_Cases.Status_Kind is
      begin
         if Code = 0 then
            return Test_Cases.Success;
         else
            return Test_Cases.Failure;
         end if;
      end Code_To_Status;

      use type League.Calendars.Date_Time;

      Started : constant League.Calendars.Date_Time :=
        League.Calendars.Clock;

      Gprbuild  : constant League.Strings.Universal_String :=
        +"gprbuild";

      Arguments : League.String_Vectors.Universal_String_Vector;

      Code   : Integer;
   begin
      Arguments.Append (+"-j0");
      Arguments.Append (+"-p");
      Arguments.Append (+"-m");
      Arguments.Append (+"-aP");
      Arguments.Append (Self.Source & "/gnat");

      for J in 1 .. Self.Search.Length loop
         Arguments.Append (+"-aP");
         Arguments.Append (Self.Search.Element (J));
      end loop;

      Arguments.Append ("-XGELA_BUILD=" & Self.Build);
      Arguments.Append (+"-P");
      Arguments.Append (Self.Project);
      Arguments.Append (Self.Options);

      Host.Execute
        (Gprbuild, Arguments, Code, Self.Output, Self.Output_File);

      Self.Status := Code_To_Status (Code);

      Self.Duration := League.Calendars.Clock - Started;
   end Run;

   ------------
   -- Search --
   ------------

   function Search
     (Self : Test_Case) return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Search;
   end Search;

   ----------------
   -- Set_Search --
   ----------------

   procedure Set_Search
     (Self  : in out Test_Case;
      Value : League.String_Vectors.Universal_String_Vector) is
   begin
      Self.Search := Value;
   end Set_Search;

   -----------------
   -- Set_Options --
   -----------------

   procedure Set_Options
     (Self  : in out Test_Case;
      Value : League.String_Vectors.Universal_String_Vector) is
   begin
      Self.Options := Value;
   end Set_Options;

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

end Gela.Build_Test_Cases;
