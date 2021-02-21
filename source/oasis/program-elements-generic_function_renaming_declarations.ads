--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Generic_Function_Renaming_Declarations is

   pragma Pure (Program.Elements.Generic_Function_Renaming_Declarations);

   type Generic_Function_Renaming_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generic_Function_Renaming_Declaration_Access is
     access all Generic_Function_Renaming_Declaration'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Generic_Function_Renaming_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Renamed_Function
    (Self : Generic_Function_Renaming_Declaration)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Aspects
    (Self : Generic_Function_Renaming_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Generic_Function_Renaming_Declaration_Text is limited interface;

   type Generic_Function_Renaming_Declaration_Text_Access is
     access all Generic_Function_Renaming_Declaration_Text'Class
     with Storage_Size => 0;

   not overriding function To_Generic_Function_Renaming_Declaration_Text
    (Self : in out Generic_Function_Renaming_Declaration)
      return Generic_Function_Renaming_Declaration_Text_Access is abstract;

   not overriding function Generic_Token
    (Self : Generic_Function_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Function_Token
    (Self : Generic_Function_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Renames_Token
    (Self : Generic_Function_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Generic_Function_Renaming_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Generic_Function_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Generic_Function_Renaming_Declarations;
