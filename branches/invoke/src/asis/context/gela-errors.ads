------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Types;

package Gela.Errors is
   pragma Preelaborate;

   type Error_Handler is limited interface;

   type Error_Handler_Access is access all Error_Handler'Class;

   not overriding procedure File_Not_Found
     (Self      : access Error_Handler;
      File_Name : League.Strings.Universal_String) is abstract;
   --  Can't lookup file passed in parameters

   not overriding procedure No_Compilation_Unit_Body
     (Self      : access Error_Handler;
      Unit_Name : League.Strings.Universal_String) is abstract;
   --  Body of a unit not provided and not found.

   not overriding procedure No_Compilation_Unit_Declaration
     (Self      : access Error_Handler;
      Unit_Name : League.Strings.Universal_String) is abstract;
   --  Declaration of a unit not provided and not found.

   not overriding procedure Not_In_NFKC_Warning
     (Self        : access Error_Handler;
      Compilation : Gela.Types.Compilation_Access) is abstract;
   --  Text of compilation is not in Normalization Form KC. ARM 4.1/3

   not overriding procedure Singe_File_Expected
     (Self : access Error_Handler) is abstract;
   --  Only one file should be passed in parameters

   not overriding procedure Syntax_Error
     (Self      : access Error_Handler;
      File_Name : League.Strings.Universal_String) is abstract;
   --  Syntax error while parsing file

end Gela.Errors;
