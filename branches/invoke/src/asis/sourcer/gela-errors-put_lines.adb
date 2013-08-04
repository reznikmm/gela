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
