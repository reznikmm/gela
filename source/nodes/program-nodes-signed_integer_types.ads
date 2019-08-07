--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Signed_Integer_Types;
with Program.Element_Visitors;

package Program.Nodes.Signed_Integer_Types is

   pragma Preelaborate;

   type Signed_Integer_Type is
     new Program.Nodes.Node
         and Program.Elements.Signed_Integer_Types.Signed_Integer_Type
         and Program.Elements.Signed_Integer_Types.Signed_Integer_Type_Text
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
      return Signed_Integer_Type;

   type Implicit_Signed_Integer_Type is
     new Program.Nodes.Node
         and Program.Elements.Signed_Integer_Types.Signed_Integer_Type
     with private;

   function Create
    (Lower_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Signed_Integer_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Signed_Integer_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Signed_Integer_Types.Signed_Integer_Type
     with record
        Lower_Bound : not null Program.Elements.Expressions.Expression_Access;
        Upper_Bound : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Signed_Integer_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Signed_Integer_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Lower_Bound
    (Self : Base_Signed_Integer_Type)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Upper_Bound
    (Self : Base_Signed_Integer_Type)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Signed_Integer_Type
    (Self : Base_Signed_Integer_Type)
      return Boolean;

   overriding function Is_Type_Definition
    (Self : Base_Signed_Integer_Type)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Signed_Integer_Type)
      return Boolean;

   type Signed_Integer_Type is
     new Base_Signed_Integer_Type
       and Program.Elements.Signed_Integer_Types.Signed_Integer_Type_Text
     with record
        Range_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Double_Dot_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Signed_Integer_Type_Text
    (Self : aliased in out Signed_Integer_Type)
      return Program.Elements.Signed_Integer_Types
          .Signed_Integer_Type_Text_Access;

   overriding function Range_Token
    (Self : Signed_Integer_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Double_Dot_Token
    (Self : Signed_Integer_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Signed_Integer_Type is
     new Base_Signed_Integer_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Signed_Integer_Type_Text
    (Self : aliased in out Implicit_Signed_Integer_Type)
      return Program.Elements.Signed_Integer_Types
          .Signed_Integer_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Signed_Integer_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Signed_Integer_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Signed_Integer_Type)
      return Boolean;

end Program.Nodes.Signed_Integer_Types;
