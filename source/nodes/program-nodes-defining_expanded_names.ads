--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Expanded_Names;
with Program.Element_Visitors;

package Program.Nodes.Defining_Expanded_Names is

   pragma Pure (Program.Nodes.Defining_Expanded_Names);

   type Defining_Expanded_Name is
     new Program.Nodes.Node
         and Program.Elements.Defining_Expanded_Names.Defining_Expanded_Name
         and Program.Elements.Defining_Expanded_Names
           .Defining_Expanded_Name_Text
     with private;

   function Create
    (Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return Defining_Expanded_Name;

   type Implicit_Defining_Expanded_Name is
     new Program.Nodes.Node
         and Program.Elements.Defining_Expanded_Names.Defining_Expanded_Name
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Selector             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Defining_Expanded_Name
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Defining_Expanded_Name is
     abstract new Program.Nodes.Node
       and Program.Elements.Defining_Expanded_Names.Defining_Expanded_Name
     with record
        Prefix   : not null Program.Elements.Expressions.Expression_Access;
        Selector : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Defining_Expanded_Name'Class);

   overriding procedure Visit
    (Self    : not null access Base_Defining_Expanded_Name;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Prefix
    (Self : Base_Defining_Expanded_Name)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Selector
    (Self : Base_Defining_Expanded_Name)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Is_Defining_Expanded_Name
    (Self : Base_Defining_Expanded_Name)
      return Boolean;

   overriding function Is_Defining_Name
    (Self : Base_Defining_Expanded_Name)
      return Boolean;

   type Defining_Expanded_Name is
     new Base_Defining_Expanded_Name
       and Program.Elements.Defining_Expanded_Names.Defining_Expanded_Name_Text
     with record
        Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Defining_Expanded_Name_Text
    (Self : aliased in out Defining_Expanded_Name)
      return Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Text_Access;

   overriding function Dot_Token
    (Self : Defining_Expanded_Name)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Defining_Expanded_Name is
     new Base_Defining_Expanded_Name
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Defining_Expanded_Name_Text
    (Self : aliased in out Implicit_Defining_Expanded_Name)
      return Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Defining_Expanded_Name)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Defining_Expanded_Name)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Defining_Expanded_Name)
      return Boolean;

end Program.Nodes.Defining_Expanded_Names;
