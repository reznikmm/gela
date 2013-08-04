------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

package Gela.Errors.Put_Lines is

   type Handler is new Error_Handler with null record;

   overriding procedure Not_In_NFKC_Warning
     (Self        : access Handler;
      Compilation : Gela.Types.Compilation_Access);
   --  Text of compilation is not in Normalization Form KC. ARM 4.1/3

   overriding procedure File_Not_Found
     (Self      : access Handler;
      File_Name : League.Strings.Universal_String);
   --  Can't lookup file passed in parameters

   overriding procedure Syntax_Error
     (Self      : access Handler;
      File_Name : League.Strings.Universal_String);
   --  Syntax error while parsing file

   overriding procedure Singe_File_Expected (Self : access Handler);
   --  Only one file should be passed in parameters

end Gela.Errors.Put_Lines;
