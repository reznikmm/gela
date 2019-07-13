--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Task_Body_Stubs is

   pragma Pure (Program.Elements.Task_Body_Stubs);

   type Task_Body_Stub is
     limited interface and Program.Elements.Declarations.Declaration;

   type Task_Body_Stub_Access is access all Task_Body_Stub'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Task_Body_Stub)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Aspects
    (Self : Task_Body_Stub)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Task_Body_Stub_Text is limited interface;

   type Task_Body_Stub_Text_Access is access all Task_Body_Stub_Text'Class
     with Storage_Size => 0;

   not overriding function To_Task_Body_Stub_Text
    (Self : aliased in out Task_Body_Stub)
      return Task_Body_Stub_Text_Access is abstract;

   not overriding function Task_Token
    (Self : Task_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Body_Token
    (Self : Task_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Task_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Separate_Token
    (Self : Task_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Task_Body_Stub_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Task_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Task_Body_Stubs;
