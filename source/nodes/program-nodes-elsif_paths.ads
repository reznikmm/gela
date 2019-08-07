--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Elsif_Paths;
with Program.Element_Visitors;

package Program.Nodes.Elsif_Paths is

   pragma Preelaborate;

   type Elsif_Path is
     new Program.Nodes.Node and Program.Elements.Elsif_Paths.Elsif_Path
         and Program.Elements.Elsif_Paths.Elsif_Path_Text
     with private;

   function Create
    (Elsif_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Condition   : not null Program.Elements.Expressions.Expression_Access;
     Then_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return Elsif_Path;

   type Implicit_Elsif_Path is
     new Program.Nodes.Node and Program.Elements.Elsif_Paths.Elsif_Path
     with private;

   function Create
    (Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Elsif_Path
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Elsif_Path is
     abstract new Program.Nodes.Node
       and Program.Elements.Elsif_Paths.Elsif_Path
     with record
        Condition  : not null Program.Elements.Expressions.Expression_Access;
        Statements : not null Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Elsif_Path'Class);

   overriding procedure Visit
    (Self    : not null access Base_Elsif_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Condition
    (Self : Base_Elsif_Path)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Statements
    (Self : Base_Elsif_Path)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Elsif_Path (Self : Base_Elsif_Path) return Boolean;

   overriding function Is_Path (Self : Base_Elsif_Path) return Boolean;

   type Elsif_Path is
     new Base_Elsif_Path and Program.Elements.Elsif_Paths.Elsif_Path_Text
     with record
        Elsif_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        Then_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Elsif_Path_Text
    (Self : aliased in out Elsif_Path)
      return Program.Elements.Elsif_Paths.Elsif_Path_Text_Access;

   overriding function Elsif_Token
    (Self : Elsif_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Then_Token
    (Self : Elsif_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Elsif_Path is
     new Base_Elsif_Path
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Elsif_Path_Text
    (Self : aliased in out Implicit_Elsif_Path)
      return Program.Elements.Elsif_Paths.Elsif_Path_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Elsif_Path)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Elsif_Path)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Elsif_Path)
      return Boolean;

end Program.Nodes.Elsif_Paths;
