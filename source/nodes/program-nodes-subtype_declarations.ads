--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Subtype_Indications;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Subtype_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Subtype_Declarations is

   pragma Pure (Program.Nodes.Subtype_Declarations);

   type Subtype_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Subtype_Declarations.Subtype_Declaration
         and Program.Elements.Subtype_Declarations.Subtype_Declaration_Text
     with private;

   function Create
    (Subtype_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Subtype_Declaration;

   type Implicit_Subtype_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Subtype_Declarations.Subtype_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Subtype_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Subtype_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Subtype_Declarations.Subtype_Declaration
     with record
        Name               : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Subtype_Indication : not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;
        Aspects            : not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Subtype_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Subtype_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Subtype_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Subtype_Indication
    (Self : Base_Subtype_Declaration)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;

   overriding function Aspects
    (Self : Base_Subtype_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Subtype_Declaration
    (Self : Base_Subtype_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Subtype_Declaration)
      return Boolean;

   type Subtype_Declaration is
     new Base_Subtype_Declaration
       and Program.Elements.Subtype_Declarations.Subtype_Declaration_Text
     with record
        Subtype_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Subtype_Declaration_Text
    (Self : aliased in out Subtype_Declaration)
      return Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Text_Access;

   overriding function Subtype_Token
    (Self : Subtype_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Subtype_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Subtype_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Subtype_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Subtype_Declaration is
     new Base_Subtype_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Subtype_Declaration_Text
    (Self : aliased in out Implicit_Subtype_Declaration)
      return Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Subtype_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Subtype_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Subtype_Declaration)
      return Boolean;

end Program.Nodes.Subtype_Declarations;
