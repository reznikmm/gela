with Asis.Text;

package Asis.Extensions.Errors is

   type Error_Handler is abstract tagged null record;

   type Error_Kind is
     (Syntax_Error);

   procedure Report_Error
     (Object      : in out Error_Handler;
      Kind        : in     Error_Kind;
      The_Unit    : in     Compilation_Unit := Asis.Nil_Compilation_Unit;
      File_Name   : in     Wide_String             := "";
      Line        : in     Text.Line_Number        := 0;
      Column      : in     Text.Character_Position := 0;
      Text        : in     Wide_String             := "") is abstract;


   type Error_Handler_Access is access all Error_Handler'Class;

   procedure Set_Error_Handler
     (Context : in out Asis.Context;
      Handler : in     Error_Handler_Access);

end Asis.Extensions.Errors;
