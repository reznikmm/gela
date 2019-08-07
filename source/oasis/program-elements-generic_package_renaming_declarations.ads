--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Generic_Package_Renaming_Declarations is

   pragma Pure (Program.Elements.Generic_Package_Renaming_Declarations);

   type Generic_Package_Renaming_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generic_Package_Renaming_Declaration_Access is
     access all Generic_Package_Renaming_Declaration'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Generic_Package_Renaming_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Renamed_Package
    (Self : Generic_Package_Renaming_Declaration)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Aspects
    (Self : Generic_Package_Renaming_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Generic_Package_Renaming_Declaration_Text is limited interface;

   type Generic_Package_Renaming_Declaration_Text_Access is
     access all Generic_Package_Renaming_Declaration_Text'Class
     with Storage_Size => 0;

   not overriding function To_Generic_Package_Renaming_Declaration_Text
    (Self : aliased in out Generic_Package_Renaming_Declaration)
      return Generic_Package_Renaming_Declaration_Text_Access is abstract;

   not overriding function Generic_Token
    (Self : Generic_Package_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Package_Token
    (Self : Generic_Package_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Renames_Token
    (Self : Generic_Package_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Generic_Package_Renaming_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Generic_Package_Renaming_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Generic_Package_Renaming_Declarations;
