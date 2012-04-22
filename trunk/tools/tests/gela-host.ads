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
      Output_File : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Directory   : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String);
   --  Execute Command with Arguments in Directory, place execution output
   --  to Output_File and return it's content as Output parameter.
   --  Return execution status in Exit_Code.
   --  If Command is simple file name, then search Command in PATH.
   --  If Output_File is empty, use unique file name.
   --  If Output_File is simple file name, when place it in temp directory.

end Gela.Host;
