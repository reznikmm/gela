--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Real_Range_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Real_Range_Specifications is

   pragma Preelaborate;

   type Real_Range_Specification is
     new Program.Nodes.Node
         and Program.Elements.Real_Range_Specifications
           .Real_Range_Specification
         and Program.Elements.Real_Range_Specifications
           .Real_Range_Specification_Text
     with private;

   function Create
    (Range_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Lower_Bound      : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound      : not null Program.Elements.Expressions
         .Expression_Access)
      return Real_Range_Specification;

   type Implicit_Real_Range_Specification is
     new Program.Nodes.Node
         and Program.Elements.Real_Range_Specifications
           .Real_Range_Specification
     with private;

   function Create
    (Lower_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Real_Range_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Real_Range_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Real_Range_Specifications.Real_Range_Specification
     with record
        Lower_Bound : not null Program.Elements.Expressions.Expression_Access;
        Upper_Bound : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Real_Range_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Real_Range_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Lower_Bound
    (Self : Base_Real_Range_Specification)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Upper_Bound
    (Self : Base_Real_Range_Specification)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Real_Range_Specification_Element
    (Self : Base_Real_Range_Specification)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Real_Range_Specification)
      return Boolean;

   type Real_Range_Specification is
     new Base_Real_Range_Specification
       and Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Text
     with record
        Range_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Double_Dot_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Real_Range_Specification_Text
    (Self : aliased in out Real_Range_Specification)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Text_Access;

   overriding function Range_Token
    (Self : Real_Range_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Double_Dot_Token
    (Self : Real_Range_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Real_Range_Specification is
     new Base_Real_Range_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Real_Range_Specification_Text
    (Self : aliased in out Implicit_Real_Range_Specification)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Real_Range_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Real_Range_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Real_Range_Specification)
      return Boolean;

end Program.Nodes.Real_Range_Specifications;
