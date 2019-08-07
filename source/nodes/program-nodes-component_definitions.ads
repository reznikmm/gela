--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Component_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Component_Definitions is

   pragma Preelaborate;

   type Component_Definition is
     new Program.Nodes.Node
         and Program.Elements.Component_Definitions.Component_Definition
         and Program.Elements.Component_Definitions.Component_Definition_Text
     with private;

   function Create
    (Aliased_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Element_Access)
      return Component_Definition;

   type Implicit_Component_Definition is
     new Program.Nodes.Node
         and Program.Elements.Component_Definitions.Component_Definition
     with private;

   function Create
    (Subtype_Indication   : not null Program.Elements.Element_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Aliased          : Boolean := False)
      return Implicit_Component_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Component_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Component_Definitions.Component_Definition
     with record
        Subtype_Indication : not null Program.Elements.Element_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Component_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Component_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subtype_Indication
    (Self : Base_Component_Definition)
      return not null Program.Elements.Element_Access;

   overriding function Is_Component_Definition
    (Self : Base_Component_Definition)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Component_Definition)
      return Boolean;

   type Component_Definition is
     new Base_Component_Definition
       and Program.Elements.Component_Definitions.Component_Definition_Text
     with record
        Aliased_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Component_Definition_Text
    (Self : aliased in out Component_Definition)
      return Program.Elements.Component_Definitions
          .Component_Definition_Text_Access;

   overriding function Aliased_Token
    (Self : Component_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Aliased
    (Self : Component_Definition)
      return Boolean;

   type Implicit_Component_Definition is
     new Base_Component_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Aliased          : Boolean;
     end record;

   overriding function To_Component_Definition_Text
    (Self : aliased in out Implicit_Component_Definition)
      return Program.Elements.Component_Definitions
          .Component_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Component_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Component_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Component_Definition)
      return Boolean;

   overriding function Has_Aliased
    (Self : Implicit_Component_Definition)
      return Boolean;

end Program.Nodes.Component_Definitions;
