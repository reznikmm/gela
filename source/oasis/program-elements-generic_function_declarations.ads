--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Defining_Names;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Generic_Function_Declarations is

   pragma Pure (Program.Elements.Generic_Function_Declarations);

   type Generic_Function_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generic_Function_Declaration_Access is
     access all Generic_Function_Declaration'Class with Storage_Size => 0;

   not overriding function Formal_Parameters
    (Self : Generic_Function_Declaration)
      return Program.Element_Vectors.Element_Vector_Access is abstract;

   not overriding function Name
    (Self : Generic_Function_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Parameters
    (Self : Generic_Function_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Result_Subtype
    (Self : Generic_Function_Declaration)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Aspects
    (Self : Generic_Function_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Generic_Function_Declaration)
      return Boolean is abstract;

   type Generic_Function_Declaration_Text is limited interface;

   type Generic_Function_Declaration_Text_Access is
     access all Generic_Function_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Generic_Function_Declaration_Text
    (Self : in out Generic_Function_Declaration)
      return Generic_Function_Declaration_Text_Access is abstract;

   not overriding function Generic_Token
    (Self : Generic_Function_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Function_Token
    (Self : Generic_Function_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Generic_Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Generic_Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Return_Token
    (Self : Generic_Function_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Not_Token
    (Self : Generic_Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Generic_Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token
    (Self : Generic_Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Generic_Function_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Generic_Function_Declarations;
