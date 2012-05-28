with Print_Element;
with Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;

package body Actions.Print is
   
   procedure Execute
     (Self    : in out Print_Action;
      Element : Asis.Element)
   is
      use Ada.Strings.Wide_Fixed;
   begin
      Ada.Wide_Text_IO.Put (Self.Indent * ' ');
      Print_Element (Element);
      Ada.Wide_Text_IO.New_Line;
      Self.Indent := Self.Indent + 2;
   end Execute;

   procedure Leave
     (Self    : in out Print_Action;
      Element : Asis.Element)
   is
      pragma Unreferenced (Element);
   begin
      Self.Indent := Self.Indent - 2;
   end Leave;

end Actions.Print;
