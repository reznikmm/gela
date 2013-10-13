------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with Gela.Compilations;

package body Gela.Errors.Put_Lines is

   function Image (Position : Gela.Lexical.Position) return Wide_Wide_String;

   W : constant Wide_Wide_String := "warning: ";

   --------------------
   -- File_Not_Found --
   --------------------

   overriding procedure File_Not_Found
     (Self      : access Handler;
      File_Name : League.Strings.Universal_String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Can't lookup file passed in parameters: " &
           File_Name.To_Wide_Wide_String);
   end File_Not_Found;

   -----------
   -- Image --
   -----------

   function Image (Position : Gela.Lexical.Position) return Wide_Wide_String
   is
      Line_Image : Wide_Wide_String :=
        Gela.Lexical.Line_Count'Wide_Wide_Image (Position.Line);
      Colon_Image : Wide_Wide_String :=
        Gela.Lexical.Colon_Count'Wide_Wide_Image (Position.Colon);
   begin
      Line_Image (1) := ':';
      Colon_Image (1) := ':';
      return Line_Image & Colon_Image;
   end Image;

   -----------------
   -- Lexer_Error --
   -----------------

   overriding procedure Lexer_Error
     (Self      : access Handler;
      File_Name : League.Strings.Universal_String;
      Position  : Gela.Lexical.Position)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Error while lexic analysis of file: " &
           File_Name.To_Wide_Wide_String & Image (Position));
   end Lexer_Error;

   ------------------------------
   -- No_Compilation_Unit_Body --
   ------------------------------

   overriding procedure No_Compilation_Unit_Body
     (Self      : access Handler;
      Unit_Name : League.Strings.Universal_String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Body of a unit not provided and not found: " &
           Unit_Name.To_Wide_Wide_String);
   end No_Compilation_Unit_Body;

   -------------------------------------
   -- No_Compilation_Unit_Declaration --
   -------------------------------------

   overriding procedure No_Compilation_Unit_Declaration
     (Self      : access Handler;
      Unit_Name : League.Strings.Universal_String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Declaration of a unit not provided and not found: " &
           Unit_Name.To_Wide_Wide_String);
   end No_Compilation_Unit_Declaration;

   -------------------------
   -- Not_In_NFKC_Warning --
   -------------------------

   overriding procedure Not_In_NFKC_Warning
     (Self        : access Handler;
      Compilation : Gela.Types.Compilation_Access)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (Gela.Compilations.Text_Name (Compilation).To_Wide_Wide_String &
         ": " & W &
         "Text of compilation is not in Normalization Form KC. ARM 4.1/3");
   end Not_In_NFKC_Warning;

   -------------------------
   -- Singe_File_Expected --
   -------------------------

   overriding procedure Singe_File_Expected (Self : access Handler) is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Singe file expected");
   end Singe_File_Expected;

   ------------------
   -- Syntax_Error --
   ------------------

   overriding procedure Syntax_Error
     (Self      : access Handler;
      File_Name : League.Strings.Universal_String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Syntax error while parsing file: " & File_Name.To_Wide_Wide_String);
   end Syntax_Error;

end Gela.Errors.Put_Lines;
