--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Real_Range_Specifications;
with Program.Elements.Decimal_Fixed_Point_Types;
with Program.Element_Visitors;

package Program.Nodes.Decimal_Fixed_Point_Types is

   pragma Preelaborate;

   type Decimal_Fixed_Point_Type is
     new Program.Nodes.Node
         and Program.Elements.Decimal_Fixed_Point_Types
           .Decimal_Fixed_Point_Type
         and Program.Elements.Decimal_Fixed_Point_Types
           .Decimal_Fixed_Point_Type_Text
     with private;

   function Create
    (Delta_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression  : not null Program.Elements.Expressions
         .Expression_Access;
     Digits_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range        : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return Decimal_Fixed_Point_Type;

   type Implicit_Decimal_Fixed_Point_Type is
     new Program.Nodes.Node
         and Program.Elements.Decimal_Fixed_Point_Types
           .Decimal_Fixed_Point_Type
     with private;

   function Create
    (Delta_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Digits_Expression    : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range           : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Decimal_Fixed_Point_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Decimal_Fixed_Point_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Type
     with record
        Delta_Expression  : not null Program.Elements.Expressions
          .Expression_Access;
        Digits_Expression : not null Program.Elements.Expressions
          .Expression_Access;
        Real_Range        : Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Decimal_Fixed_Point_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Decimal_Fixed_Point_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Delta_Expression
    (Self : Base_Decimal_Fixed_Point_Type)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Digits_Expression
    (Self : Base_Decimal_Fixed_Point_Type)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Real_Range
    (Self : Base_Decimal_Fixed_Point_Type)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access;

   overriding function Is_Decimal_Fixed_Point_Type_Element
    (Self : Base_Decimal_Fixed_Point_Type)
      return Boolean;

   overriding function Is_Type_Definition_Element
    (Self : Base_Decimal_Fixed_Point_Type)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Decimal_Fixed_Point_Type)
      return Boolean;

   type Decimal_Fixed_Point_Type is
     new Base_Decimal_Fixed_Point_Type
       and Program.Elements.Decimal_Fixed_Point_Types
         .Decimal_Fixed_Point_Type_Text
     with record
        Delta_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Digits_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Decimal_Fixed_Point_Type_Text
    (Self : aliased in out Decimal_Fixed_Point_Type)
      return Program.Elements.Decimal_Fixed_Point_Types
          .Decimal_Fixed_Point_Type_Text_Access;

   overriding function Delta_Token
    (Self : Decimal_Fixed_Point_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Digits_Token
    (Self : Decimal_Fixed_Point_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Decimal_Fixed_Point_Type is
     new Base_Decimal_Fixed_Point_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Decimal_Fixed_Point_Type_Text
    (Self : aliased in out Implicit_Decimal_Fixed_Point_Type)
      return Program.Elements.Decimal_Fixed_Point_Types
          .Decimal_Fixed_Point_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Decimal_Fixed_Point_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Decimal_Fixed_Point_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Decimal_Fixed_Point_Type)
      return Boolean;

end Program.Nodes.Decimal_Fixed_Point_Types;
