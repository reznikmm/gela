--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Incomplete_Type_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Incomplete_Type_Definitions is

   pragma Pure (Program.Nodes.Incomplete_Type_Definitions);

   type Incomplete_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Incomplete_Type_Definitions
           .Incomplete_Type_Definition
         and Program.Elements.Incomplete_Type_Definitions
           .Incomplete_Type_Definition_Text
     with private;

   function Create
    (Tagged_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Incomplete_Type_Definition;

   type Implicit_Incomplete_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Incomplete_Type_Definitions
           .Incomplete_Type_Definition
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Tagged           : Boolean := False)
      return Implicit_Incomplete_Type_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Incomplete_Type_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Incomplete_Type_Definitions
         .Incomplete_Type_Definition
     with null record;

   procedure Initialize
    (Self : aliased in out Base_Incomplete_Type_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Incomplete_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Incomplete_Type_Definition
    (Self : Base_Incomplete_Type_Definition)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Incomplete_Type_Definition)
      return Boolean;

   type Incomplete_Type_Definition is
     new Base_Incomplete_Type_Definition
       and Program.Elements.Incomplete_Type_Definitions
         .Incomplete_Type_Definition_Text
     with record
        Tagged_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Incomplete_Type_Definition_Text
    (Self : aliased in out Incomplete_Type_Definition)
      return Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Text_Access;

   overriding function Tagged_Token
    (Self : Incomplete_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Tagged
    (Self : Incomplete_Type_Definition)
      return Boolean;

   type Implicit_Incomplete_Type_Definition is
     new Base_Incomplete_Type_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Tagged           : Boolean;
     end record;

   overriding function To_Incomplete_Type_Definition_Text
    (Self : aliased in out Implicit_Incomplete_Type_Definition)
      return Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean;

   overriding function Has_Tagged
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean;

end Program.Nodes.Incomplete_Type_Definitions;
