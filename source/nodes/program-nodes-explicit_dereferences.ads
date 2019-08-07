--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Explicit_Dereferences;
with Program.Element_Visitors;

package Program.Nodes.Explicit_Dereferences is

   pragma Preelaborate;

   type Explicit_Dereference is
     new Program.Nodes.Node
         and Program.Elements.Explicit_Dereferences.Explicit_Dereference
         and Program.Elements.Explicit_Dereferences.Explicit_Dereference_Text
     with private;

   function Create
    (Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     All_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Explicit_Dereference;

   type Implicit_Explicit_Dereference is
     new Program.Nodes.Node
         and Program.Elements.Explicit_Dereferences.Explicit_Dereference
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Explicit_Dereference
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Explicit_Dereference is
     abstract new Program.Nodes.Node
       and Program.Elements.Explicit_Dereferences.Explicit_Dereference
     with record
        Prefix : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Explicit_Dereference'Class);

   overriding procedure Visit
    (Self    : not null access Base_Explicit_Dereference;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Prefix
    (Self : Base_Explicit_Dereference)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Explicit_Dereference
    (Self : Base_Explicit_Dereference)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Explicit_Dereference)
      return Boolean;

   type Explicit_Dereference is
     new Base_Explicit_Dereference
       and Program.Elements.Explicit_Dereferences.Explicit_Dereference_Text
     with record
        Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        All_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Explicit_Dereference_Text
    (Self : aliased in out Explicit_Dereference)
      return Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Text_Access;

   overriding function Dot_Token
    (Self : Explicit_Dereference)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function All_Token
    (Self : Explicit_Dereference)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Explicit_Dereference is
     new Base_Explicit_Dereference
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Explicit_Dereference_Text
    (Self : aliased in out Implicit_Explicit_Dereference)
      return Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Explicit_Dereference)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Explicit_Dereference)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Explicit_Dereference)
      return Boolean;

end Program.Nodes.Explicit_Dereferences;
