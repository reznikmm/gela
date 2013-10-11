------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body Gela.Test_Cases.Base is

   --------------
   -- Duration --
   --------------

   overriding function Duration
     (Self : Test_Case) return League.Calendars.Time is
   begin
      return Self.Spent;
   end Duration;

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Info.File;
   end File;

   -------------
   -- Fixture --
   -------------

   overriding function Fixture
     (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Info.Fixture;
   end Fixture;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Test_Case) return League.Strings.Universal_String is
   begin
      return Self.Info.Name;
   end Name;

   ---------
   -- Run --
   ---------

   overriding procedure Run (Self : in out Test_Case) is
      use type League.Calendars.Date_Time;

      Started : constant League.Calendars.Date_Time :=
        League.Calendars.Clock;
   begin
      Test_Case'Class (Self).Execute;
      Self.Spent := League.Calendars.Clock - Started;
   end Run;

   ---------------------
   -- Set_Information --
   ---------------------

   procedure Set_Information
     (Self : in out Test_Case;
      Info : Base_Information) is
   begin
      Self.Info := Info;
   end Set_Information;

end Gela.Test_Cases.Base;
