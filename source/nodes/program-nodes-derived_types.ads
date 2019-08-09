--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Derived_Types;
with Program.Element_Visitors;

package Program.Nodes.Derived_Types is

   pragma Preelaborate;

   type Derived_Type is
     new Program.Nodes.Node and Program.Elements.Derived_Types.Derived_Type
         and Program.Elements.Derived_Types.Derived_Type_Text
     with private;

   function Create
    (Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Parent         : not null Program.Elements.Expressions.Expression_Access)
      return Derived_Type;

   type Implicit_Derived_Type is
     new Program.Nodes.Node and Program.Elements.Derived_Types.Derived_Type
     with private;

   function Create
    (Parent               : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False)
      return Implicit_Derived_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Derived_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Derived_Types.Derived_Type
     with record
        Parent : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Derived_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Derived_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Parent
    (Self : Base_Derived_Type)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Derived_Type_Element
    (Self : Base_Derived_Type)
      return Boolean;

   overriding function Is_Type_Definition_Element
    (Self : Base_Derived_Type)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Derived_Type)
      return Boolean;

   type Derived_Type is
     new Base_Derived_Type and Program.Elements.Derived_Types.Derived_Type_Text
     with record
        Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        New_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Derived_Type_Text
    (Self : aliased in out Derived_Type)
      return Program.Elements.Derived_Types.Derived_Type_Text_Access;

   overriding function Abstract_Token
    (Self : Derived_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Limited_Token
    (Self : Derived_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function New_Token
    (Self : Derived_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Abstract (Self : Derived_Type) return Boolean;

   overriding function Has_Limited (Self : Derived_Type) return Boolean;

   type Implicit_Derived_Type is
     new Base_Derived_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Abstract         : Boolean;
        Has_Limited          : Boolean;
     end record;

   overriding function To_Derived_Type_Text
    (Self : aliased in out Implicit_Derived_Type)
      return Program.Elements.Derived_Types.Derived_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Derived_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Derived_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Derived_Type)
      return Boolean;

   overriding function Has_Abstract
    (Self : Implicit_Derived_Type)
      return Boolean;

   overriding function Has_Limited
    (Self : Implicit_Derived_Type)
      return Boolean;

end Program.Nodes.Derived_Types;
