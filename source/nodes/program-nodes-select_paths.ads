--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Select_Paths;
with Program.Element_Visitors;

package Program.Nodes.Select_Paths is

   pragma Preelaborate;

   type Select_Path is
     new Program.Nodes.Node and Program.Elements.Select_Paths.Select_Path
         and Program.Elements.Select_Paths.Select_Path_Text
     with private;

   function Create
    (When_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Guard       : Program.Elements.Expressions.Expression_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return Select_Path;

   type Implicit_Select_Path is
     new Program.Nodes.Node and Program.Elements.Select_Paths.Select_Path
     with private;

   function Create
    (Guard                : Program.Elements.Expressions.Expression_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Select_Path
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Select_Path is
     abstract new Program.Nodes.Node
       and Program.Elements.Select_Paths.Select_Path
     with record
        Guard      : Program.Elements.Expressions.Expression_Access;
        Statements : not null Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Select_Path'Class);

   overriding procedure Visit
    (Self    : not null access Base_Select_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Guard
    (Self : Base_Select_Path)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Statements
    (Self : Base_Select_Path)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Select_Path_Element
    (Self : Base_Select_Path)
      return Boolean;

   overriding function Is_Path_Element
    (Self : Base_Select_Path)
      return Boolean;

   type Select_Path is
     new Base_Select_Path and Program.Elements.Select_Paths.Select_Path_Text
     with record
        When_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Select_Path_Text
    (Self : aliased in out Select_Path)
      return Program.Elements.Select_Paths.Select_Path_Text_Access;

   overriding function When_Token
    (Self : Select_Path)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Arrow_Token
    (Self : Select_Path)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Select_Path is
     new Base_Select_Path
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Select_Path_Text
    (Self : aliased in out Implicit_Select_Path)
      return Program.Elements.Select_Paths.Select_Path_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Select_Path)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Select_Path)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Select_Path)
      return Boolean;

end Program.Nodes.Select_Paths;
