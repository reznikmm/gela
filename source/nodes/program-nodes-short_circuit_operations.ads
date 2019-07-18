--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Short_Circuit_Operations;
with Program.Element_Visitors;

package Program.Nodes.Short_Circuit_Operations is

   pragma Pure (Program.Nodes.Short_Circuit_Operations);

   type Short_Circuit_Operation is
     new Program.Nodes.Node
         and Program.Elements.Short_Circuit_Operations.Short_Circuit_Operation
         and Program.Elements.Short_Circuit_Operations
           .Short_Circuit_Operation_Text
     with private;

   function Create
    (Left       : not null Program.Elements.Expressions.Expression_Access;
     And_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Or_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Right      : not null Program.Elements.Expressions.Expression_Access)
      return Short_Circuit_Operation;

   type Implicit_Short_Circuit_Operation is
     new Program.Nodes.Node
         and Program.Elements.Short_Circuit_Operations.Short_Circuit_Operation
     with private;

   function Create
    (Left                 : not null Program.Elements.Expressions
         .Expression_Access;
     Right                : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_And_Then         : Boolean := False;
     Has_Or_Else          : Boolean := False)
      return Implicit_Short_Circuit_Operation
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Short_Circuit_Operation is
     abstract new Program.Nodes.Node
       and Program.Elements.Short_Circuit_Operations.Short_Circuit_Operation
     with record
        Left  : not null Program.Elements.Expressions.Expression_Access;
        Right : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Short_Circuit_Operation'Class);

   overriding procedure Visit
    (Self    : not null access Base_Short_Circuit_Operation;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Left
    (Self : Base_Short_Circuit_Operation)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Right
    (Self : Base_Short_Circuit_Operation)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Short_Circuit_Operation
    (Self : Base_Short_Circuit_Operation)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Short_Circuit_Operation)
      return Boolean;

   type Short_Circuit_Operation is
     new Base_Short_Circuit_Operation
       and Program.Elements.Short_Circuit_Operations
         .Short_Circuit_Operation_Text
     with record
        And_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Then_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Or_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        Else_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Short_Circuit_Operation_Text
    (Self : aliased in out Short_Circuit_Operation)
      return Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Text_Access;

   overriding function And_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Then_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Or_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Else_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_And_Then
    (Self : Short_Circuit_Operation)
      return Boolean;

   overriding function Has_Or_Else
    (Self : Short_Circuit_Operation)
      return Boolean;

   type Implicit_Short_Circuit_Operation is
     new Base_Short_Circuit_Operation
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_And_Then         : Boolean;
        Has_Or_Else          : Boolean;
     end record;

   overriding function To_Short_Circuit_Operation_Text
    (Self : aliased in out Implicit_Short_Circuit_Operation)
      return Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean;

   overriding function Has_And_Then
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean;

   overriding function Has_Or_Else
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean;

end Program.Nodes.Short_Circuit_Operations;
