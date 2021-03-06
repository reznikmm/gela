--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Defining_Names;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Expressions;

package Program.Elements.Generic_Package_Declarations is

   pragma Pure (Program.Elements.Generic_Package_Declarations);

   type Generic_Package_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generic_Package_Declaration_Access is
     access all Generic_Package_Declaration'Class with Storage_Size => 0;

   not overriding function Formal_Parameters
    (Self : Generic_Package_Declaration)
      return Program.Element_Vectors.Element_Vector_Access is abstract;

   not overriding function Name
    (Self : Generic_Package_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Aspects
    (Self : Generic_Package_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Visible_Declarations
    (Self : Generic_Package_Declaration)
      return Program.Element_Vectors.Element_Vector_Access is abstract;

   not overriding function Private_Declarations
    (Self : Generic_Package_Declaration)
      return Program.Element_Vectors.Element_Vector_Access is abstract;

   not overriding function End_Name
    (Self : Generic_Package_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Generic_Package_Declaration_Text is limited interface;

   type Generic_Package_Declaration_Text_Access is
     access all Generic_Package_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Generic_Package_Declaration_Text
    (Self : in out Generic_Package_Declaration)
      return Generic_Package_Declaration_Text_Access is abstract;

   not overriding function Generic_Token
    (Self : Generic_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Package_Token
    (Self : Generic_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Generic_Package_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Is_Token
    (Self : Generic_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Private_Token
    (Self : Generic_Package_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function End_Token
    (Self : Generic_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Generic_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Generic_Package_Declarations;
