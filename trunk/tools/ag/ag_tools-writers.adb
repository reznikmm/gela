------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package body AG_Tools.Writers is

   New_Line : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);

   procedure N (Self : in out Writer; Text : Wide_Wide_String) is
   begin
      Self.Text.Append (Text);
   end N;

   procedure N
     (Self : in out Writer;
      Text : League.Strings.Universal_String) is
   begin
      Self.N (Text.To_Wide_Wide_String);
   end N;

   procedure N
     (Self : in out Writer;
      Text : Wide_Wide_String;
      Copy : in out Writer'Class) is
   begin
      Self.N (Text);
      Copy.N (Text);
   end N;

   procedure N
     (Self : in out Writer;
      Text : League.Strings.Universal_String;
      Copy : in out Writer'Class) is
   begin
      Self.N (Text);
      Copy.N (Text);
   end N;

   -------
   -- N --
   -------

   procedure N
     (Self  : in out Writer;
      Value : Natural)
   is
      Image : constant Wide_Wide_String := Natural'Wide_Wide_Image (Value);
   begin
      Self.N (Image (2 .. Image'Last));
   end N;

   procedure P
     (Self : in out Writer;
      Text : Wide_Wide_String := "";
      Copy : in out Writer'Class) is
   begin
      Self.P (Text);
      Copy.P (Text);
   end P;

   procedure P
     (Self   : in out Writer;
      Text   : League.Strings.Universal_String;
      Copy : in out Writer'Class) is
   begin
      Self.P (Text);
      Copy.P (Text);
   end P;

   procedure P
     (Self   : in out Writer;
      Text   : League.Strings.Universal_String) is
   begin
      Self.P (Text.To_Wide_Wide_String);
   end P;

   procedure P
     (Self   : in out Writer;
      Text   : Wide_Wide_String := "") is
   begin
      Self.Text.Append (Text);
      Self.Text.Append (New_Line);
   end P;

end AG_Tools.Writers;
