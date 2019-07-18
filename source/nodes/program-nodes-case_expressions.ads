--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Case_Expression_Paths;
with Program.Elements.Case_Expressions;
with Program.Element_Visitors;

package Program.Nodes.Case_Expressions is

   pragma Pure (Program.Nodes.Case_Expressions);

   type Case_Expression is
     new Program.Nodes.Node
         and Program.Elements.Case_Expressions.Case_Expression
         and Program.Elements.Case_Expressions.Case_Expression_Text
     with private;

   function Create
    (Case_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Vector_Access)
      return Case_Expression;

   type Implicit_Case_Expression is
     new Program.Nodes.Node
         and Program.Elements.Case_Expressions.Case_Expression
     with private;

   function Create
    (Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Paths                : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Expression
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Case_Expression is
     abstract new Program.Nodes.Node
       and Program.Elements.Case_Expressions.Case_Expression
     with record
        Selecting_Expression : not null Program.Elements.Expressions
          .Expression_Access;
        Paths                : not null Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Case_Expression'Class);

   overriding procedure Visit
    (Self    : not null access Base_Case_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Selecting_Expression
    (Self : Base_Case_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Paths
    (Self : Base_Case_Expression)
      return not null Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Vector_Access;

   overriding function Is_Case_Expression
    (Self : Base_Case_Expression)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Case_Expression)
      return Boolean;

   type Case_Expression is
     new Base_Case_Expression
       and Program.Elements.Case_Expressions.Case_Expression_Text
     with record
        Case_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        Is_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Case_Expression_Text
    (Self : aliased in out Case_Expression)
      return Program.Elements.Case_Expressions.Case_Expression_Text_Access;

   overriding function Case_Token
    (Self : Case_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Case_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Case_Expression is
     new Base_Case_Expression
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Case_Expression_Text
    (Self : aliased in out Implicit_Case_Expression)
      return Program.Elements.Case_Expressions.Case_Expression_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Expression)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Expression)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Expression)
      return Boolean;

end Program.Nodes.Case_Expressions;
