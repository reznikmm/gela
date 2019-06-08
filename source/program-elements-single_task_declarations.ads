--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Task_Definitions;

package Program.Elements.Single_Task_Declarations is

   pragma Pure (Program.Elements.Single_Task_Declarations);

   type Single_Task_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Single_Task_Declaration_Access is
     access all Single_Task_Declaration'Class with Storage_Size => 0;

   not overriding function Task_Token
    (Self : Single_Task_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Single_Task_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function With_Token
    (Self : Single_Task_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Single_Task_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Is_Token
    (Self : Single_Task_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Single_Task_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Progenitors
    (Self : Single_Task_Declaration)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function With_Token_2
    (Self : Single_Task_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Definition
    (Self : Single_Task_Declaration)
      return not null Program.Elements.Task_Definitions.Task_Definition_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Single_Task_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Single_Task_Declarations;
