--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Slices;
with Program.Element_Visitors;

package Program.Nodes.Slices is

   pragma Preelaborate;

   type Slice is
     new Program.Nodes.Node and Program.Elements.Slices.Slice
         and Program.Elements.Slices.Slice_Text
     with private;

   function Create
    (Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Slice_Range         : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Slice;

   type Implicit_Slice is
     new Program.Nodes.Node and Program.Elements.Slices.Slice
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Slice_Range          : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Slice
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Slice is
     abstract new Program.Nodes.Node and Program.Elements.Slices.Slice
     with record
        Prefix      : not null Program.Elements.Expressions.Expression_Access;
        Slice_Range : not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Slice'Class);

   overriding procedure Visit
    (Self    : not null access Base_Slice;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Prefix
    (Self : Base_Slice)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Slice_Range
    (Self : Base_Slice)
      return not null Program.Elements.Discrete_Ranges.Discrete_Range_Access;

   overriding function Is_Slice_Element (Self : Base_Slice) return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Slice)
      return Boolean;

   type Slice is
     new Base_Slice and Program.Elements.Slices.Slice_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Slice_Text
    (Self : aliased in out Slice)
      return Program.Elements.Slices.Slice_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Slice)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Slice)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Slice is
     new Base_Slice
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Slice_Text
    (Self : aliased in out Implicit_Slice)
      return Program.Elements.Slices.Slice_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Slice)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Slice)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Slice)
      return Boolean;

end Program.Nodes.Slices;
