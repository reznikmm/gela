------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with tests for for Gela project,            --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Characters.Conversions;
with League.Text_Codecs;

package body Gela.Conv is

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Text : League.Strings.Universal_String)
      return String
   renames League.Text_Codecs.To_Exception_Message;

   -------------------------
   -- To_Universal_String --
   -------------------------

   function To_Universal_String
     (Text : String)
      return League.Strings.Universal_String
   is
      Wide_Text : constant Wide_Wide_String :=
        Ada.Characters.Conversions.To_Wide_Wide_String (Text);
   begin
      return League.Strings.To_Universal_String (Wide_Text);
   end To_Universal_String;

end Gela.Conv;
