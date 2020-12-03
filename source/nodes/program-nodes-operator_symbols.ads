--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Operator_Symbols;
with Program.Element_Visitors;

package Program.Nodes.Operator_Symbols is

   pragma Preelaborate;

   type Operator_Symbol is
     new Program.Nodes.Node
         and Program.Elements.Operator_Symbols.Operator_Symbol
         and Program.Elements.Operator_Symbols.Operator_Symbol_Text
     with private;

   function Create
    (Operator_Symbol_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Operator_Symbol;

   type Implicit_Operator_Symbol is
     new Program.Nodes.Node
         and Program.Elements.Operator_Symbols.Operator_Symbol
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Operator_Symbol
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Operator_Symbol is
     abstract new Program.Nodes.Node
       and Program.Elements.Operator_Symbols.Operator_Symbol
     with null record;

   procedure Initialize (Self : aliased in out Base_Operator_Symbol'Class);

   overriding procedure Visit
    (Self    : not null access Base_Operator_Symbol;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Operator_Symbol_Element
    (Self : Base_Operator_Symbol)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Operator_Symbol)
      return Boolean;

   type Operator_Symbol is
     new Base_Operator_Symbol
       and Program.Elements.Operator_Symbols.Operator_Symbol_Text
     with record
        Operator_Symbol_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Operator_Symbol_Text
    (Self : aliased in out Operator_Symbol)
      return Program.Elements.Operator_Symbols.Operator_Symbol_Text_Access;

   overriding function Operator_Symbol_Token
    (Self : Operator_Symbol)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Image (Self : Operator_Symbol) return Text;

   type Implicit_Operator_Symbol is
     new Base_Operator_Symbol
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Operator_Symbol_Text
    (Self : aliased in out Implicit_Operator_Symbol)
      return Program.Elements.Operator_Symbols.Operator_Symbol_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Operator_Symbol)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Operator_Symbol)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Operator_Symbol)
      return Boolean;

   overriding function Image (Self : Implicit_Operator_Symbol) return Text;

end Program.Nodes.Operator_Symbols;
