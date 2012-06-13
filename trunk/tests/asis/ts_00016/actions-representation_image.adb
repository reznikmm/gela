with Ada.Wide_Text_IO;
with Asis.Declarations;
with Asis.Elements;
with Print_Element;

package body Actions.Representation_Image is

   procedure Execute
     (Self    : in out Action;
      Element : Asis.Element)
   is
      pragma Unreferenced (Self);
   begin
      case Asis.Elements.Defining_Name_Kind (Element) is
         when Asis.A_Defining_Character_Literal |
           Asis.A_Defining_Enumeration_Literal =>
            null;
         when others =>
            return;
      end case;

      Print_Element (Element);

      Ada.Wide_Text_IO.Put_Line
        (Asis.Declarations.Representation_Value_Image (Element));
   end Execute;

end Actions.Representation_Image;
