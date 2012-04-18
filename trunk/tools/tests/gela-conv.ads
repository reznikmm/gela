------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

package Gela.Conv is

   function To_Universal_String
     (Text : String) return League.Strings.Universal_String;

   function To_String
     (Text : League.Strings.Universal_String) return String;

end Gela.Conv;

