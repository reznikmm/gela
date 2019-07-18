--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Case_Paths;
with Program.Element_Visitors;

package Program.Nodes.Case_Paths is

   pragma Pure (Program.Nodes.Case_Paths);

   type Case_Path is
     new Program.Nodes.Node and Program.Elements.Case_Paths.Case_Path
         and Program.Elements.Case_Paths.Case_Path_Text
     with private;

   function Create
    (When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return Case_Path;

   type Implicit_Case_Path is
     new Program.Nodes.Node and Program.Elements.Case_Paths.Case_Path
     with private;

   function Create
    (Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Path
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Case_Path is
     abstract new Program.Nodes.Node and Program.Elements.Case_Paths.Case_Path
     with record
        Choices    : not null Program.Element_Vectors.Element_Vector_Access;
        Statements : not null Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Case_Path'Class);

   overriding procedure Visit
    (Self    : not null access Base_Case_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Choices
    (Self : Base_Case_Path)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Statements
    (Self : Base_Case_Path)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Case_Path (Self : Base_Case_Path) return Boolean;

   overriding function Is_Path (Self : Base_Case_Path) return Boolean;

   type Case_Path is
     new Base_Case_Path and Program.Elements.Case_Paths.Case_Path_Text
     with record
        When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Case_Path_Text
    (Self : aliased in out Case_Path)
      return Program.Elements.Case_Paths.Case_Path_Text_Access;

   overriding function When_Token
    (Self : Case_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Arrow_Token
    (Self : Case_Path)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Case_Path is
     new Base_Case_Path
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Case_Path_Text
    (Self : aliased in out Implicit_Case_Path)
      return Program.Elements.Case_Paths.Case_Path_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Path)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Path)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Path)
      return Boolean;

end Program.Nodes.Case_Paths;
