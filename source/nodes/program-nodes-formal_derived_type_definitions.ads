--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Formal_Derived_Type_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Formal_Derived_Type_Definitions is

   pragma Preelaborate;

   type Formal_Derived_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Formal_Derived_Type_Definitions
           .Formal_Derived_Type_Definition
         and Program.Elements.Formal_Derived_Type_Definitions
           .Formal_Derived_Type_Definition_Text
     with private;

   function Create
    (Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Mark       : not null Program.Elements.Expressions
         .Expression_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token      : Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Derived_Type_Definition;

   type Implicit_Formal_Derived_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Formal_Derived_Type_Definitions
           .Formal_Derived_Type_Definition
     with private;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Progenitors          : Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Synchronized     : Boolean := False;
     Has_With_Private     : Boolean := False)
      return Implicit_Formal_Derived_Type_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Derived_Type_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Derived_Type_Definitions
         .Formal_Derived_Type_Definition
     with record
        Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
        Progenitors  : Program.Elements.Expressions.Expression_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Derived_Type_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Derived_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subtype_Mark
    (Self : Base_Formal_Derived_Type_Definition)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Progenitors
    (Self : Base_Formal_Derived_Type_Definition)
      return Program.Elements.Expressions.Expression_Vector_Access;

   overriding function Is_Formal_Derived_Type_Definition
    (Self : Base_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Is_Formal_Type_Definition
    (Self : Base_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Formal_Derived_Type_Definition)
      return Boolean;

   type Formal_Derived_Type_Definition is
     new Base_Formal_Derived_Type_Definition
       and Program.Elements.Formal_Derived_Type_Definitions
         .Formal_Derived_Type_Definition_Text
     with record
        Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
        New_Token          : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
        Private_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Formal_Derived_Type_Definition_Text
    (Self : aliased in out Formal_Derived_Type_Definition)
      return Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Text_Access;

   overriding function Abstract_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Limited_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Synchronized_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function New_Token
    (Self : Formal_Derived_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function And_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Private_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Abstract
    (Self : Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Has_Limited
    (Self : Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Has_Synchronized
    (Self : Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Has_With_Private
    (Self : Formal_Derived_Type_Definition)
      return Boolean;

   type Implicit_Formal_Derived_Type_Definition is
     new Base_Formal_Derived_Type_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Abstract         : Boolean;
        Has_Limited          : Boolean;
        Has_Synchronized     : Boolean;
        Has_With_Private     : Boolean;
     end record;

   overriding function To_Formal_Derived_Type_Definition_Text
    (Self : aliased in out Implicit_Formal_Derived_Type_Definition)
      return Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Has_Abstract
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Has_Limited
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Has_Synchronized
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean;

   overriding function Has_With_Private
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean;

end Program.Nodes.Formal_Derived_Type_Definitions;
