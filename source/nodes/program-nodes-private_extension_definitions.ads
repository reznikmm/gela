--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;
with Program.Elements.Expressions;
with Program.Elements.Private_Extension_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Private_Extension_Definitions is

   pragma Preelaborate;

   type Private_Extension_Definition is
     new Program.Nodes.Node
         and Program.Elements.Private_Extension_Definitions
           .Private_Extension_Definition
         and Program.Elements.Private_Extension_Definitions
           .Private_Extension_Definition_Text
     with private;

   function Create
    (Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ancestor           : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Private_Extension_Definition;

   type Implicit_Private_Extension_Definition is
     new Program.Nodes.Node
         and Program.Elements.Private_Extension_Definitions
           .Private_Extension_Definition
     with private;

   function Create
    (Ancestor             : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Progenitors          : Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Synchronized     : Boolean := False)
      return Implicit_Private_Extension_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Private_Extension_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Private_Extension_Definitions
         .Private_Extension_Definition
     with record
        Ancestor    : not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;
        Progenitors : Program.Elements.Expressions.Expression_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Private_Extension_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Private_Extension_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Ancestor
    (Self : Base_Private_Extension_Definition)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;

   overriding function Progenitors
    (Self : Base_Private_Extension_Definition)
      return Program.Elements.Expressions.Expression_Vector_Access;

   overriding function Is_Private_Extension_Definition_Element
    (Self : Base_Private_Extension_Definition)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Private_Extension_Definition)
      return Boolean;

   type Private_Extension_Definition is
     new Base_Private_Extension_Definition
       and Program.Elements.Private_Extension_Definitions
         .Private_Extension_Definition_Text
     with record
        Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
        New_Token          : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token         : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Private_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Private_Extension_Definition_Text
    (Self : aliased in out Private_Extension_Definition)
      return Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Text_Access;

   overriding function Abstract_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Limited_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Synchronized_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function New_Token
    (Self : Private_Extension_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function And_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Private_Extension_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Private_Token
    (Self : Private_Extension_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Abstract
    (Self : Private_Extension_Definition)
      return Boolean;

   overriding function Has_Limited
    (Self : Private_Extension_Definition)
      return Boolean;

   overriding function Has_Synchronized
    (Self : Private_Extension_Definition)
      return Boolean;

   type Implicit_Private_Extension_Definition is
     new Base_Private_Extension_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Abstract         : Boolean;
        Has_Limited          : Boolean;
        Has_Synchronized     : Boolean;
     end record;

   overriding function To_Private_Extension_Definition_Text
    (Self : aliased in out Implicit_Private_Extension_Definition)
      return Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Private_Extension_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Private_Extension_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Private_Extension_Definition)
      return Boolean;

   overriding function Has_Abstract
    (Self : Implicit_Private_Extension_Definition)
      return Boolean;

   overriding function Has_Limited
    (Self : Implicit_Private_Extension_Definition)
      return Boolean;

   overriding function Has_Synchronized
    (Self : Implicit_Private_Extension_Definition)
      return Boolean;

end Program.Nodes.Private_Extension_Definitions;
