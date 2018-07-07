------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with League.String_Vectors;

package Gela.Host is

   procedure Execute
     (Command     : League.Strings.Universal_String;
      Arguments   : League.String_Vectors.Universal_String_Vector :=
        League.String_Vectors.Empty_Universal_String_Vector;
      Exit_Code   : out Integer;
      Output      : out League.Strings.Universal_String;
      Directory   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String);
   --  Execute Command with Arguments in Directory, place execution output
   --  to Output parameter.
   --  Return execution status in Exit_Code.
   --  If Command is simple file name, then search Command in PATH.

   function Build_Root return League.Strings.Universal_String;
   --  Build root is a directory where gela-test_driver was built

   function Source_Root return League.Strings.Universal_String;
   --  Source root is a directory where tools/tests/gela-host.adb locates

end Gela.Host;
