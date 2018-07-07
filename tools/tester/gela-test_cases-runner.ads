------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Test_Cases.Base;
with League.String_Vectors;

package Gela.Test_Cases.Runner is

   type Test_Case is new Base.Test_Case with private;
   type Test_Case_Access is access all Test_Case'Class;

   function Create
     (Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Directory : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Info      : Base.Base_Information := Base.No_Info)
      return Test_Case;

private

   type Test_Case is new Base.Test_Case with record
      Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Directory : League.Strings.Universal_String;
      Status    : Status_Kind;
      Output    : League.Strings.Universal_String;
      Traceback : League.Strings.Universal_String;
   end record;

   overriding procedure Execute (Self : in out Test_Case);

   overriding function Status (Self : Test_Case) return Status_Kind;

   overriding function Output
     (Self : Test_Case) return League.Strings.Universal_String;

   overriding function Traceback
     (Self : Test_Case) return League.Strings.Universal_String;

end Gela.Test_Cases.Runner;
