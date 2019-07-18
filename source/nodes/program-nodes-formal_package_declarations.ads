--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Expressions;
with Program.Elements.Formal_Package_Associations;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Formal_Package_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Formal_Package_Declarations is

   pragma Pure (Program.Nodes.Formal_Package_Declarations);

   type Formal_Package_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Formal_Package_Declarations
           .Formal_Package_Declaration
         and Program.Elements.Formal_Package_Declarations
           .Formal_Package_Declaration_Text
     with private;

   function Create
    (With_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters           : not null Program.Elements
         .Formal_Package_Associations.Formal_Package_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Formal_Package_Declaration;

   type Implicit_Formal_Package_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Formal_Package_Declarations
           .Formal_Package_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements
         .Formal_Package_Associations.Formal_Package_Association_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Package_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Package_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Package_Declarations
         .Formal_Package_Declaration
     with record
        Name                 : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Generic_Package_Name : not null Program.Elements.Expressions
          .Expression_Access;
        Parameters           : not null Program.Elements
          .Formal_Package_Associations
          .Formal_Package_Association_Vector_Access;
        Aspects              : not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Package_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Package_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Generic_Package_Name
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Parameters
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Vector_Access;

   overriding function Aspects
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Formal_Package_Declaration
    (Self : Base_Formal_Package_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Formal_Package_Declaration)
      return Boolean;

   type Formal_Package_Declaration is
     new Base_Formal_Package_Declaration
       and Program.Elements.Formal_Package_Declarations
         .Formal_Package_Declaration_Text
     with record
        With_Token          : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Package_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token            : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        New_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Formal_Package_Declaration_Text
    (Self : aliased in out Formal_Package_Declaration)
      return Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Text_Access;

   overriding function With_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Package_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function New_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Formal_Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Formal_Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token_2
    (Self : Formal_Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Formal_Package_Declaration is
     new Base_Formal_Package_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Formal_Package_Declaration_Text
    (Self : aliased in out Implicit_Formal_Package_Declaration)
      return Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Package_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Package_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Package_Declaration)
      return Boolean;

end Program.Nodes.Formal_Package_Declarations;
