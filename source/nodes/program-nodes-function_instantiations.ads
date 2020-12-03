--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Elements.Parameter_Associations;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Function_Instantiations;
with Program.Element_Visitors;

package Program.Nodes.Function_Instantiations is

   pragma Preelaborate;

   type Function_Instantiation is
     new Program.Nodes.Node
         and Program.Elements.Function_Instantiations.Function_Instantiation
         and Program.Elements.Function_Instantiations
           .Function_Instantiation_Text
     with private;

   function Create
    (Not_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Is_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Function_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters            : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects               : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Function_Instantiation;

   type Implicit_Function_Instantiation is
     new Program.Nodes.Node
         and Program.Elements.Function_Instantiations.Function_Instantiation
     with private;

   function Create
    (Name                  : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Generic_Function_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters            : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Aspects               : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False;
     Has_Not               : Boolean := False;
     Has_Overriding        : Boolean := False)
      return Implicit_Function_Instantiation
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Function_Instantiation is
     abstract new Program.Nodes.Node
       and Program.Elements.Function_Instantiations.Function_Instantiation
     with record
        Name                  : not null Program.Elements.Defining_Names
          .Defining_Name_Access;
        Generic_Function_Name : not null Program.Elements.Expressions
          .Expression_Access;
        Parameters            : Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;
        Aspects               : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Function_Instantiation'Class);

   overriding procedure Visit
    (Self    : not null access Base_Function_Instantiation;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Function_Instantiation)
      return not null Program.Elements.Defining_Names.Defining_Name_Access;

   overriding function Generic_Function_Name
    (Self : Base_Function_Instantiation)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Parameters
    (Self : Base_Function_Instantiation)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;

   overriding function Aspects
    (Self : Base_Function_Instantiation)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Function_Instantiation_Element
    (Self : Base_Function_Instantiation)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Function_Instantiation)
      return Boolean;

   type Function_Instantiation is
     new Base_Function_Instantiation
       and Program.Elements.Function_Instantiations.Function_Instantiation_Text
     with record
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        Function_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token            : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        New_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Function_Instantiation_Text
    (Self : aliased in out Function_Instantiation)
      return Program.Elements.Function_Instantiations
          .Function_Instantiation_Text_Access;

   overriding function Not_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Overriding_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Function_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function New_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not (Self : Function_Instantiation) return Boolean;

   overriding function Has_Overriding
    (Self : Function_Instantiation)
      return Boolean;

   type Implicit_Function_Instantiation is
     new Base_Function_Instantiation
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not              : Boolean;
        Has_Overriding       : Boolean;
     end record;

   overriding function To_Function_Instantiation_Text
    (Self : aliased in out Implicit_Function_Instantiation)
      return Program.Elements.Function_Instantiations
          .Function_Instantiation_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Function_Instantiation)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Function_Instantiation)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Function_Instantiation)
      return Boolean;

   overriding function Has_Not
    (Self : Implicit_Function_Instantiation)
      return Boolean;

   overriding function Has_Overriding
    (Self : Implicit_Function_Instantiation)
      return Boolean;

end Program.Nodes.Function_Instantiations;
