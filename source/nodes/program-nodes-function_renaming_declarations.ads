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
with Program.Elements.Function_Renaming_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Function_Renaming_Declarations is

   pragma Pure (Program.Nodes.Function_Renaming_Declarations);

   type Function_Renaming_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Function_Renaming_Declarations
           .Function_Renaming_Declaration
         and Program.Elements.Function_Renaming_Declarations
           .Function_Renaming_Declaration_Text
     with private;

   function Create
    (Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     Renames_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Renamed_Function    : Program.Elements.Expressions.Expression_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Function_Renaming_Declaration;

   type Implicit_Function_Renaming_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Function_Renaming_Declarations
           .Function_Renaming_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Renamed_Function     : Program.Elements.Expressions.Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Function_Renaming_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Function_Renaming_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Function_Renaming_Declarations
         .Function_Renaming_Declaration
     with record
        Name             : not null Program.Elements.Defining_Names
          .Defining_Name_Access;
        Parameters       : not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Result_Subtype   : not null Program.Elements.Element_Access;
        Renamed_Function : Program.Elements.Expressions.Expression_Access;
        Aspects          : not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Function_Renaming_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Function_Renaming_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Function_Renaming_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access;

   overriding function Parameters
    (Self : Base_Function_Renaming_Declaration)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Result_Subtype
    (Self : Base_Function_Renaming_Declaration)
      return not null Program.Elements.Element_Access;

   overriding function Renamed_Function
    (Self : Base_Function_Renaming_Declaration)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Aspects
    (Self : Base_Function_Renaming_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Function_Renaming_Declaration
    (Self : Base_Function_Renaming_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Function_Renaming_Declaration)
      return Boolean;

   type Function_Renaming_Declaration is
     new Base_Function_Renaming_Declaration
       and Program.Elements.Function_Renaming_Declarations
         .Function_Renaming_Declaration_Text
     with record
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        Function_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Return_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Renames_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Function_Renaming_Declaration_Text
    (Self : aliased in out Function_Renaming_Declaration)
      return Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration_Text_Access;

   overriding function Not_Token
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Overriding_Token
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Function_Token
    (Self : Function_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Return_Token
    (Self : Function_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Not_Token_2
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Renames_Token
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Function_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Function_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not
    (Self : Function_Renaming_Declaration)
      return Boolean;

   overriding function Has_Overriding
    (Self : Function_Renaming_Declaration)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Function_Renaming_Declaration)
      return Boolean;

   type Implicit_Function_Renaming_Declaration is
     new Base_Function_Renaming_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not              : Boolean;
        Has_Overriding       : Boolean;
        Has_Not_Null         : Boolean;
     end record;

   overriding function To_Function_Renaming_Declaration_Text
    (Self : aliased in out Implicit_Function_Renaming_Declaration)
      return Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Function_Renaming_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Function_Renaming_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Function_Renaming_Declaration)
      return Boolean;

   overriding function Has_Not
    (Self : Implicit_Function_Renaming_Declaration)
      return Boolean;

   overriding function Has_Overriding
    (Self : Implicit_Function_Renaming_Declaration)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Function_Renaming_Declaration)
      return Boolean;

end Program.Nodes.Function_Renaming_Declarations;
