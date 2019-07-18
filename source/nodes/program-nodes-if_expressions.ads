--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Elsif_Paths;
with Program.Elements.If_Expressions;
with Program.Element_Visitors;

package Program.Nodes.If_Expressions is

   pragma Pure (Program.Nodes.If_Expressions);

   type If_Expression is
     new Program.Nodes.Node and Program.Elements.If_Expressions.If_Expression
         and Program.Elements.If_Expressions.If_Expression_Text
     with private;

   function Create
    (If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Expression : not null Program.Elements.Expressions.Expression_Access;
     Elsif_Paths     : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Expression : Program.Elements.Expressions.Expression_Access)
      return If_Expression;

   type Implicit_If_Expression is
     new Program.Nodes.Node and Program.Elements.If_Expressions.If_Expression
     with private;

   function Create
    (Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Then_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Elsif_Paths          : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Expression      : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_If_Expression
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_If_Expression is
     abstract new Program.Nodes.Node
       and Program.Elements.If_Expressions.If_Expression
     with record
        Condition       : not null Program.Elements.Expressions
          .Expression_Access;
        Then_Expression : not null Program.Elements.Expressions
          .Expression_Access;
        Elsif_Paths     : not null Program.Elements.Elsif_Paths
          .Elsif_Path_Vector_Access;
        Else_Expression : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_If_Expression'Class);

   overriding procedure Visit
    (Self    : not null access Base_If_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Condition
    (Self : Base_If_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Then_Expression
    (Self : Base_If_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Elsif_Paths
    (Self : Base_If_Expression)
      return not null Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access;

   overriding function Else_Expression
    (Self : Base_If_Expression)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_If_Expression
    (Self : Base_If_Expression)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_If_Expression)
      return Boolean;

   type If_Expression is
     new Base_If_Expression
       and Program.Elements.If_Expressions.If_Expression_Text
     with record
        If_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
        Then_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        Else_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_If_Expression_Text
    (Self : aliased in out If_Expression)
      return Program.Elements.If_Expressions.If_Expression_Text_Access;

   overriding function If_Token
    (Self : If_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Then_Token
    (Self : If_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Else_Token
    (Self : If_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_If_Expression is
     new Base_If_Expression
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_If_Expression_Text
    (Self : aliased in out Implicit_If_Expression)
      return Program.Elements.If_Expressions.If_Expression_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_If_Expression)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_If_Expression)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_If_Expression)
      return Boolean;

end Program.Nodes.If_Expressions;
