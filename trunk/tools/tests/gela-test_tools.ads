------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

package Gela.Test_Tools is

   function Read_File
     (File_Name : League.Strings.Universal_String)
     return League.Strings.Universal_String;

   function Read_File
     (File_Name : String) return League.Strings.Universal_String;

   procedure Write_File
     (File : League.Strings.Universal_String;
      Text : League.Strings.Universal_String);

end Gela.Test_Tools;

