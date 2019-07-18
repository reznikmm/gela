--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Others_Choices;
with Program.Element_Visitors;

package Program.Nodes.Others_Choices is

   pragma Pure (Program.Nodes.Others_Choices);

   type Others_Choice is
     new Program.Nodes.Node and Program.Elements.Others_Choices.Others_Choice
         and Program.Elements.Others_Choices.Others_Choice_Text
     with private;

   function Create
    (Others_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Others_Choice;

   type Implicit_Others_Choice is
     new Program.Nodes.Node and Program.Elements.Others_Choices.Others_Choice
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Others_Choice
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Others_Choice is
     abstract new Program.Nodes.Node
       and Program.Elements.Others_Choices.Others_Choice
     with null record;

   procedure Initialize (Self : aliased in out Base_Others_Choice'Class);

   overriding procedure Visit
    (Self    : not null access Base_Others_Choice;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Others_Choice
    (Self : Base_Others_Choice)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Others_Choice)
      return Boolean;

   type Others_Choice is
     new Base_Others_Choice
       and Program.Elements.Others_Choices.Others_Choice_Text
     with record
        Others_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Others_Choice_Text
    (Self : aliased in out Others_Choice)
      return Program.Elements.Others_Choices.Others_Choice_Text_Access;

   overriding function Others_Token
    (Self : Others_Choice)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Others_Choice is
     new Base_Others_Choice
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Others_Choice_Text
    (Self : aliased in out Implicit_Others_Choice)
      return Program.Elements.Others_Choices.Others_Choice_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Others_Choice)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Others_Choice)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Others_Choice)
      return Boolean;

end Program.Nodes.Others_Choices;
