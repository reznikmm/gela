--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Defining_Identifiers;
with Program.Tokens;
with Program.Elements.Loop_Parameter_Specifications;
with Program.Elements.Generalized_Iterator_Specifications;
with Program.Elements.Element_Iterator_Specifications;
with Program.Element_Vectors;
with Program.Elements.Identifiers;

package Program.Elements.For_Loop_Statements is

   pragma Pure (Program.Elements.For_Loop_Statements);

   type For_Loop_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type For_Loop_Statement_Access is access all For_Loop_Statement'Class
     with Storage_Size => 0;

   not overriding function Statement_Identifier
    (Self : For_Loop_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Colon_Token
    (Self : For_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function For_Token
    (Self : For_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Loop_Parameter
    (Self : For_Loop_Statement)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access is abstract;

   not overriding function Generalized_Iterator
    (Self : For_Loop_Statement)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access is abstract;

   not overriding function Element_Iterator
    (Self : For_Loop_Statement)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access is abstract;

   not overriding function Loop_Token
    (Self : For_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Statements
    (Self : For_Loop_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function End_Token
    (Self : For_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Loop_Token_2
    (Self : For_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Statement_Identifier
    (Self : For_Loop_Statement)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Semicolon_Token
    (Self : For_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.For_Loop_Statements;
