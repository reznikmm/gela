------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

package AG_Tools.Writers is

   pragma Preelaborate;

   type Writer is tagged record
      Text : League.Strings.Universal_String;
   end record;

   procedure P
     (Self   : in out Writer;
      Text   : Wide_Wide_String := "");

   procedure N
     (Self : in out Writer;
      Text : Wide_Wide_String);

   procedure P
     (Self   : in out Writer;
      Text   : League.Strings.Universal_String);

   procedure N
     (Self : in out Writer;
      Text : League.Strings.Universal_String);

   procedure P
     (Self : in out Writer;
      Text : Wide_Wide_String := "";
      Copy : in out Writer'Class);

   procedure N
     (Self : in out Writer;
      Text : Wide_Wide_String;
      Copy : in out Writer'Class);

   procedure P
     (Self   : in out Writer;
      Text   : League.Strings.Universal_String;
      Copy : in out Writer'Class);

   procedure N
     (Self : in out Writer;
      Text : League.Strings.Universal_String;
      Copy : in out Writer'Class);

   procedure N
     (Self  : in out Writer;
      Value : Natural);

end AG_Tools.Writers;
