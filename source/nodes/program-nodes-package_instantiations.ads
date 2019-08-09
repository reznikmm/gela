--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Elements.Parameter_Associations;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Package_Instantiations;
with Program.Element_Visitors;

package Program.Nodes.Package_Instantiations is

   pragma Preelaborate;

   type Package_Instantiation is
     new Program.Nodes.Node
         and Program.Elements.Package_Instantiations.Package_Instantiation
         and Program.Elements.Package_Instantiations.Package_Instantiation_Text
     with private;

   function Create
    (Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Package_Instantiation;

   type Implicit_Package_Instantiation is
     new Program.Nodes.Node
         and Program.Elements.Package_Instantiations.Package_Instantiation
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Package_Instantiation
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Package_Instantiation is
     abstract new Program.Nodes.Node
       and Program.Elements.Package_Instantiations.Package_Instantiation
     with record
        Name                 : not null Program.Elements.Defining_Names
          .Defining_Name_Access;
        Generic_Package_Name : not null Program.Elements.Expressions
          .Expression_Access;
        Parameters           : Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;
        Aspects              : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Package_Instantiation'Class);

   overriding procedure Visit
    (Self    : not null access Base_Package_Instantiation;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Package_Instantiation)
      return not null Program.Elements.Defining_Names.Defining_Name_Access;

   overriding function Generic_Package_Name
    (Self : Base_Package_Instantiation)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Parameters
    (Self : Base_Package_Instantiation)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;

   overriding function Aspects
    (Self : Base_Package_Instantiation)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Package_Instantiation_Element
    (Self : Base_Package_Instantiation)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Package_Instantiation)
      return Boolean;

   type Package_Instantiation is
     new Base_Package_Instantiation
       and Program.Elements.Package_Instantiations.Package_Instantiation_Text
     with record
        Package_Token       : not null Program.Lexical_Elements
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

   overriding function To_Package_Instantiation_Text
    (Self : aliased in out Package_Instantiation)
      return Program.Elements.Package_Instantiations
          .Package_Instantiation_Text_Access;

   overriding function Package_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function New_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Package_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Package_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Package_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Package_Instantiation is
     new Base_Package_Instantiation
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Package_Instantiation_Text
    (Self : aliased in out Implicit_Package_Instantiation)
      return Program.Elements.Package_Instantiations
          .Package_Instantiation_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Package_Instantiation)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Package_Instantiation)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Package_Instantiation)
      return Boolean;

end Program.Nodes.Package_Instantiations;
