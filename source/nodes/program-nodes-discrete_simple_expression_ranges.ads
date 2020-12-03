--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Discrete_Simple_Expression_Ranges;
with Program.Element_Visitors;

package Program.Nodes.Discrete_Simple_Expression_Ranges is

   pragma Preelaborate;

   type Discrete_Simple_Expression_Range is
     new Program.Nodes.Node
         and Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range
         and Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Text
     with private;

   function Create
    (Lower_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Discrete_Simple_Expression_Range;

   type Implicit_Discrete_Simple_Expression_Range is
     new Program.Nodes.Node
         and Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range
     with private;

   function Create
    (Lower_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit            : Boolean := False;
     Is_Part_Of_Inherited           : Boolean := False;
     Is_Part_Of_Instance            : Boolean := False;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Implicit_Discrete_Simple_Expression_Range
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Discrete_Simple_Expression_Range is
     abstract new Program.Nodes.Node
       and Program.Elements.Discrete_Simple_Expression_Ranges
         .Discrete_Simple_Expression_Range
     with record
        Lower_Bound                    : not null Program.Elements.Expressions
          .Expression_Access;
        Upper_Bound                    : not null Program.Elements.Expressions
          .Expression_Access;
        Is_Discrete_Subtype_Definition : Boolean;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Discrete_Simple_Expression_Range'Class);

   overriding procedure Visit
    (Self    : not null access Base_Discrete_Simple_Expression_Range;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Lower_Bound
    (Self : Base_Discrete_Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Upper_Bound
    (Self : Base_Discrete_Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Discrete_Subtype_Definition
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean;

   overriding function Is_Discrete_Simple_Expression_Range_Element
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean;

   overriding function Is_Discrete_Range_Element
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Discrete_Simple_Expression_Range)
      return Boolean;

   type Discrete_Simple_Expression_Range is
     new Base_Discrete_Simple_Expression_Range
       and Program.Elements.Discrete_Simple_Expression_Ranges
         .Discrete_Simple_Expression_Range_Text
     with record
        Double_Dot_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Discrete_Simple_Expression_Range_Text
    (Self : aliased in out Discrete_Simple_Expression_Range)
      return Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range_Text_Access;

   overriding function Double_Dot_Token
    (Self : Discrete_Simple_Expression_Range)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Discrete_Simple_Expression_Range is
     new Base_Discrete_Simple_Expression_Range
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Discrete_Simple_Expression_Range_Text
    (Self : aliased in out Implicit_Discrete_Simple_Expression_Range)
      return Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discrete_Simple_Expression_Range)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discrete_Simple_Expression_Range)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discrete_Simple_Expression_Range)
      return Boolean;

end Program.Nodes.Discrete_Simple_Expression_Ranges;
