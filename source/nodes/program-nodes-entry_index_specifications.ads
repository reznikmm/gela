--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Entry_Index_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Entry_Index_Specifications is

   pragma Pure (Program.Nodes.Entry_Index_Specifications);

   type Entry_Index_Specification is
     new Program.Nodes.Node
         and Program.Elements.Entry_Index_Specifications
           .Entry_Index_Specification
         and Program.Elements.Entry_Index_Specifications
           .Entry_Index_Specification_Text
     with private;

   function Create
    (For_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Index_Subtype : not null Program.Elements
         .Discrete_Subtype_Definitions.Discrete_Subtype_Definition_Access)
      return Entry_Index_Specification;

   type Implicit_Entry_Index_Specification is
     new Program.Nodes.Node
         and Program.Elements.Entry_Index_Specifications
           .Entry_Index_Specification
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Entry_Index_Subtype  : not null Program.Elements
         .Discrete_Subtype_Definitions.Discrete_Subtype_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Entry_Index_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Entry_Index_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Entry_Index_Specifications
         .Entry_Index_Specification
     with record
        Name                : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Entry_Index_Subtype : not null Program.Elements
          .Discrete_Subtype_Definitions.Discrete_Subtype_Definition_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Entry_Index_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Entry_Index_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Entry_Index_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Entry_Index_Subtype
    (Self : Base_Entry_Index_Specification)
      return not null Program.Elements.Discrete_Subtype_Definitions
          .Discrete_Subtype_Definition_Access;

   overriding function Is_Entry_Index_Specification
    (Self : Base_Entry_Index_Specification)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Entry_Index_Specification)
      return Boolean;

   type Entry_Index_Specification is
     new Base_Entry_Index_Specification
       and Program.Elements.Entry_Index_Specifications
         .Entry_Index_Specification_Text
     with record
        For_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        In_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Entry_Index_Specification_Text
    (Self : aliased in out Entry_Index_Specification)
      return Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Text_Access;

   overriding function For_Token
    (Self : Entry_Index_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function In_Token
    (Self : Entry_Index_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Entry_Index_Specification is
     new Base_Entry_Index_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Entry_Index_Specification_Text
    (Self : aliased in out Implicit_Entry_Index_Specification)
      return Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Entry_Index_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Entry_Index_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Entry_Index_Specification)
      return Boolean;

end Program.Nodes.Entry_Index_Specifications;
