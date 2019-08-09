--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Type_Conversions;
with Program.Element_Visitors;

package Program.Nodes.Type_Conversions is

   pragma Preelaborate;

   type Type_Conversion is
     new Program.Nodes.Node
         and Program.Elements.Type_Conversions.Type_Conversion
         and Program.Elements.Type_Conversions.Type_Conversion_Text
     with private;

   function Create
    (Subtype_Mark        : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Operand             : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Type_Conversion;

   type Implicit_Type_Conversion is
     new Program.Nodes.Node
         and Program.Elements.Type_Conversions.Type_Conversion
     with private;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Operand              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Type_Conversion
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Type_Conversion is
     abstract new Program.Nodes.Node
       and Program.Elements.Type_Conversions.Type_Conversion
     with record
        Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
        Operand      : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Type_Conversion'Class);

   overriding procedure Visit
    (Self    : not null access Base_Type_Conversion;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subtype_Mark
    (Self : Base_Type_Conversion)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Operand
    (Self : Base_Type_Conversion)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Type_Conversion_Element
    (Self : Base_Type_Conversion)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Type_Conversion)
      return Boolean;

   type Type_Conversion is
     new Base_Type_Conversion
       and Program.Elements.Type_Conversions.Type_Conversion_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Type_Conversion_Text
    (Self : aliased in out Type_Conversion)
      return Program.Elements.Type_Conversions.Type_Conversion_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Type_Conversion)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Type_Conversion)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Type_Conversion is
     new Base_Type_Conversion
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Type_Conversion_Text
    (Self : aliased in out Implicit_Type_Conversion)
      return Program.Elements.Type_Conversions.Type_Conversion_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Type_Conversion)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Type_Conversion)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Type_Conversion)
      return Boolean;

end Program.Nodes.Type_Conversions;
