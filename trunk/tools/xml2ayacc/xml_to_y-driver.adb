with Ada.Command_Line;
with Ada.Text_IO;

procedure Xml_To_Y.Driver is
   package C renames Ada.Command_Line;
begin
   if C.Argument_Count /= 5 then
      Ada.Text_IO.Put_Line
        ("Usage : xml_to_y xml_hier.xml tokens.xml fixed.xml code.xml parser.y");
   else
      Run (C.Argument (1), C.Argument (2), C.Argument (3),
           C.Argument (4), C.Argument (5));
   end if;
end Xml_To_Y.Driver;
