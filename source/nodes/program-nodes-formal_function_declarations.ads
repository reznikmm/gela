--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Formal_Function_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Formal_Function_Declarations is

   pragma Pure (Program.Nodes.Formal_Function_Declarations);

   type Formal_Function_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Formal_Function_Declarations
           .Formal_Function_Declaration
         and Program.Elements.Formal_Function_Declarations
           .Formal_Function_Declaration_Text
     with private;

   function Create
    (With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Subprogram_Default  : Program.Elements.Expressions.Expression_Access;
     Box_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Formal_Function_Declaration;

   type Implicit_Formal_Function_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Formal_Function_Declarations
           .Formal_Function_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Subprogram_Default   : Program.Elements.Expressions.Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Box              : Boolean := False)
      return Implicit_Formal_Function_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Function_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Function_Declarations
         .Formal_Function_Declaration
     with record
        Name               : not null Program.Elements.Defining_Names
          .Defining_Name_Access;
        Parameters         : Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Result_Subtype     : not null Program.Elements.Element_Access;
        Subprogram_Default : Program.Elements.Expressions.Expression_Access;
        Aspects            : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Function_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Function_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Formal_Function_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access;

   overriding function Parameters
    (Self : Base_Formal_Function_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Result_Subtype
    (Self : Base_Formal_Function_Declaration)
      return not null Program.Elements.Element_Access;

   overriding function Subprogram_Default
    (Self : Base_Formal_Function_Declaration)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Aspects
    (Self : Base_Formal_Function_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Formal_Function_Declaration
    (Self : Base_Formal_Function_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Formal_Function_Declaration)
      return Boolean;

   type Formal_Function_Declaration is
     new Base_Formal_Function_Declaration
       and Program.Elements.Formal_Function_Declarations
         .Formal_Function_Declaration_Text
     with record
        With_Token          : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Function_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Return_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
        Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Box_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Formal_Function_Declaration_Text
    (Self : aliased in out Formal_Function_Declaration)
      return Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Text_Access;

   overriding function With_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Function_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Return_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Not_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Abstract_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Box_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token_2
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null
    (Self : Formal_Function_Declaration)
      return Boolean;

   overriding function Has_Abstract
    (Self : Formal_Function_Declaration)
      return Boolean;

   overriding function Has_Box
    (Self : Formal_Function_Declaration)
      return Boolean;

   type Implicit_Formal_Function_Declaration is
     new Base_Formal_Function_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
        Has_Abstract         : Boolean;
        Has_Box              : Boolean;
     end record;

   overriding function To_Formal_Function_Declaration_Text
    (Self : aliased in out Implicit_Formal_Function_Declaration)
      return Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean;

   overriding function Has_Abstract
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean;

   overriding function Has_Box
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean;

end Program.Nodes.Formal_Function_Declarations;
