--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Exception_Renaming_Declarations is

   pragma Pure (Program.Elements.Exception_Renaming_Declarations);

   type Exception_Renaming_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Exception_Renaming_Declaration_Access is
     access all Exception_Renaming_Declaration'Class with Storage_Size => 0;

   not overriding function Names
    (Self : Exception_Renaming_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is abstract;

   not overriding function Renamed_Exception
    (Self : Exception_Renaming_Declaration)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Aspects
    (Self : Exception_Renaming_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Exception_Renaming_Declaration_Text is limited interface;

   type Exception_Renaming_Declaration_Text_Access is
     access all Exception_Renaming_Declaration_Text'Class
     with Storage_Size => 0;

   not overriding function To_Exception_Renaming_Declaration_Text
    (Self : aliased in out Exception_Renaming_Declaration)
      return Exception_Renaming_Declaration_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Exception_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Exception_Token
    (Self : Exception_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Renames_Token
    (Self : Exception_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Exception_Renaming_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Exception_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Exception_Renaming_Declarations;
