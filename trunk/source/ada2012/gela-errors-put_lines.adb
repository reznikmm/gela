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

   -------------------------
   -- Not_In_NFKC_Warning --
   -------------------------

   overriding procedure Not_In_NFKC_Warning
     (Self        : access Handler;
      Compilation : access Gela.Compilations.Compilation'Class)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (Compilation.Text_Name.To_Wide_Wide_String &
         ": " & W &
         "Text of compilation is not in Normalization Form KC. ARM 4.1/3");
   end Not_In_NFKC_Warning;

end Gela.Errors.Put_Lines;
