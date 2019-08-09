--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Formal_Private_Type_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Formal_Private_Type_Definitions is

   pragma Preelaborate;

   type Formal_Private_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Formal_Private_Type_Definitions
           .Formal_Private_Type_Definition
         and Program.Elements.Formal_Private_Type_Definitions
           .Formal_Private_Type_Definition_Text
     with private;

   function Create
    (Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Tagged_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Private_Type_Definition;

   type Implicit_Formal_Private_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Formal_Private_Type_Definitions
           .Formal_Private_Type_Definition
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Tagged           : Boolean := False;
     Has_Limited          : Boolean := False)
      return Implicit_Formal_Private_Type_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Private_Type_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Private_Type_Definitions
         .Formal_Private_Type_Definition
     with null record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Private_Type_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Private_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Formal_Private_Type_Definition_Element
    (Self : Base_Formal_Private_Type_Definition)
      return Boolean;

   overriding function Is_Formal_Type_Definition_Element
    (Self : Base_Formal_Private_Type_Definition)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Formal_Private_Type_Definition)
      return Boolean;

   type Formal_Private_Type_Definition is
     new Base_Formal_Private_Type_Definition
       and Program.Elements.Formal_Private_Type_Definitions
         .Formal_Private_Type_Definition_Text
     with record
        Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Tagged_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Private_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Formal_Private_Type_Definition_Text
    (Self : aliased in out Formal_Private_Type_Definition)
      return Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Text_Access;

   overriding function Abstract_Token
    (Self : Formal_Private_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Tagged_Token
    (Self : Formal_Private_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Limited_Token
    (Self : Formal_Private_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Private_Token
    (Self : Formal_Private_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Abstract
    (Self : Formal_Private_Type_Definition)
      return Boolean;

   overriding function Has_Tagged
    (Self : Formal_Private_Type_Definition)
      return Boolean;

   overriding function Has_Limited
    (Self : Formal_Private_Type_Definition)
      return Boolean;

   type Implicit_Formal_Private_Type_Definition is
     new Base_Formal_Private_Type_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Abstract         : Boolean;
        Has_Tagged           : Boolean;
        Has_Limited          : Boolean;
     end record;

   overriding function To_Formal_Private_Type_Definition_Text
    (Self : aliased in out Implicit_Formal_Private_Type_Definition)
      return Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean;

   overriding function Has_Abstract
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean;

   overriding function Has_Tagged
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean;

   overriding function Has_Limited
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean;

end Program.Nodes.Formal_Private_Type_Definitions;
