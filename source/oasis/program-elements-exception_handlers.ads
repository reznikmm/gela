--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Lexical_Elements;
with Program.Elements.Choice_Parameter_Specifications;

package Program.Elements.Exception_Handlers is

   pragma Pure (Program.Elements.Exception_Handlers);

   type Exception_Handler is limited interface and Program.Elements.Element;

   type Exception_Handler_Access is access all Exception_Handler'Class
     with Storage_Size => 0;

   not overriding function Choice_Parameter
    (Self : Exception_Handler)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access is abstract;

   not overriding function Choices
    (Self : Exception_Handler)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Statements
    (Self : Exception_Handler)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   type Exception_Handler_Text is limited interface;

   type Exception_Handler_Text_Access is
     access all Exception_Handler_Text'Class with Storage_Size => 0;

   not overriding function To_Exception_Handler_Text
    (Self : aliased in out Exception_Handler)
      return Exception_Handler_Text_Access is abstract;

   not overriding function When_Token
    (Self : Exception_Handler_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Arrow_Token
    (Self : Exception_Handler_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   type Exception_Handler_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Exception_Handler_Vector_Access is
     access all Exception_Handler_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Exception_Handler_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Exception_Handler;

   function To_Exception_Handler
    (Self  : Exception_Handler_Vector'Class;
     Index : Positive)
      return not null Exception_Handler_Access
     is (Self.Element (Index).To_Exception_Handler);

end Program.Elements.Exception_Handlers;
