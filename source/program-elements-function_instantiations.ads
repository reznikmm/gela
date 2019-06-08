--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Elements.Parameter_Associations;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Function_Instantiations is

   pragma Pure (Program.Elements.Function_Instantiations);

   type Function_Instantiation is
     limited interface and Program.Elements.Declarations.Declaration;

   type Function_Instantiation_Access is
     access all Function_Instantiation'Class with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Overriding_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Function_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Function_Instantiation)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Is_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Generic_Function_Name
    (Self : Function_Instantiation)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parameters
    (Self : Function_Instantiation)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Function_Instantiation)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Function_Instantiation)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Function_Instantiations;
