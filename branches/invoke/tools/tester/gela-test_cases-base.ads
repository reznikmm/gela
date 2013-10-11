------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Test_Cases.Base is

   type Test_Case is abstract new Test_Cases.Test_Case with private;
   type Test_Case_Access is access all Test_Case'Class;

   not overriding procedure Execute (Self : in out Test_Case) is abstract;

   overriding procedure Run (Self : in out Test_Case);

   overriding function Duration
     (Self : Test_Case) return League.Calendars.Time;

   overriding function Name
     (Self : Test_Case) return League.Strings.Universal_String;

   overriding function Fixture
     (Self : Test_Case) return League.Strings.Universal_String;

   overriding function File
     (Self : Test_Case) return League.Strings.Universal_String;

   type Base_Information is record
      Name    : League.Strings.Universal_String;
      Fixture : League.Strings.Universal_String;
      File    : League.Strings.Universal_String;
   end record;

   No_Info : constant Base_Information;

   procedure Set_Information
     (Self : in out Test_Case;
      Info : Base_Information);

private

   type Test_Case is abstract new Test_Cases.Test_Case with record
      Info  : Base_Information;
      Spent : League.Calendars.Time;
   end record;

   No_Info : constant Base_Information := (others => <>);

end Gela.Test_Cases.Base;
