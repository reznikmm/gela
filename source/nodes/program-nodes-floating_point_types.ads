--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Real_Range_Specifications;
with Program.Elements.Floating_Point_Types;
with Program.Element_Visitors;

package Program.Nodes.Floating_Point_Types is

   pragma Pure (Program.Nodes.Floating_Point_Types);

   type Floating_Point_Type is
     new Program.Nodes.Node
         and Program.Elements.Floating_Point_Types.Floating_Point_Type
         and Program.Elements.Floating_Point_Types.Floating_Point_Type_Text
     with private;

   function Create
    (Digits_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range        : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return Floating_Point_Type;

   type Implicit_Floating_Point_Type is
     new Program.Nodes.Node
         and Program.Elements.Floating_Point_Types.Floating_Point_Type
     with private;

   function Create
    (Digits_Expression    : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range           : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Floating_Point_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Floating_Point_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Floating_Point_Types.Floating_Point_Type
     with record
        Digits_Expression : not null Program.Elements.Expressions
          .Expression_Access;
        Real_Range        : Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Floating_Point_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Floating_Point_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Digits_Expression
    (Self : Base_Floating_Point_Type)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Real_Range
    (Self : Base_Floating_Point_Type)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access;

   overriding function Is_Floating_Point_Type
    (Self : Base_Floating_Point_Type)
      return Boolean;

   overriding function Is_Type_Definition
    (Self : Base_Floating_Point_Type)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Floating_Point_Type)
      return Boolean;

   type Floating_Point_Type is
     new Base_Floating_Point_Type
       and Program.Elements.Floating_Point_Types.Floating_Point_Type_Text
     with record
        Digits_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Floating_Point_Type_Text
    (Self : aliased in out Floating_Point_Type)
      return Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Text_Access;

   overriding function Digits_Token
    (Self : Floating_Point_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Floating_Point_Type is
     new Base_Floating_Point_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Floating_Point_Type_Text
    (Self : aliased in out Implicit_Floating_Point_Type)
      return Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Floating_Point_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Floating_Point_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Floating_Point_Type)
      return Boolean;

end Program.Nodes.Floating_Point_Types;
