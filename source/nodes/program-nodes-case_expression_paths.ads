--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Expressions;
with Program.Elements.Case_Expression_Paths;
with Program.Element_Visitors;

package Program.Nodes.Case_Expression_Paths is

   pragma Preelaborate;

   type Case_Expression_Path is
     new Program.Nodes.Node
         and Program.Elements.Case_Expression_Paths.Case_Expression_Path
         and Program.Elements.Case_Expression_Paths.Case_Expression_Path_Text
     with private;

   function Create
    (When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : not null Program.Elements.Expressions.Expression_Access)
      return Case_Expression_Path;

   type Implicit_Case_Expression_Path is
     new Program.Nodes.Node
         and Program.Elements.Case_Expression_Paths.Case_Expression_Path
     with private;

   function Create
    (Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Expression_Path
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Case_Expression_Path is
     abstract new Program.Nodes.Node
       and Program.Elements.Case_Expression_Paths.Case_Expression_Path
     with record
        Choices    : not null Program.Element_Vectors.Element_Vector_Access;
        Expression : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Case_Expression_Path'Class);

   overriding procedure Visit
    (Self    : not null access Base_Case_Expression_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Choices
    (Self : Base_Case_Expression_Path)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Expression
    (Self : Base_Case_Expression_Path)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Case_Expression_Path
    (Self : Base_Case_Expression_Path)
      return Boolean;

   overriding function Is_Path
    (Self : Base_Case_Expression_Path)
      return Boolean;

   type Case_Expression_Path is
     new Base_Case_Expression_Path
       and Program.Elements.Case_Expression_Paths.Case_Expression_Path_Text
     with record
        When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
        Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Case_Expression_Path_Text
    (Self : aliased in out Case_Expression_Path)
      return Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Text_Access;

   overriding function When_Token
    (Self : Case_Expression_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Arrow_Token
    (Self : Case_Expression_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Case_Expression_Path is
     new Base_Case_Expression_Path
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Case_Expression_Path_Text
    (Self : aliased in out Implicit_Case_Expression_Path)
      return Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Expression_Path)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Expression_Path)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Expression_Path)
      return Boolean;

end Program.Nodes.Case_Expression_Paths;
