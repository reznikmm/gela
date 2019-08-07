--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Variants;
with Program.Elements.Variant_Parts;
with Program.Element_Visitors;

package Program.Nodes.Variant_Parts is

   pragma Preelaborate;

   type Variant_Part is
     new Program.Nodes.Node and Program.Elements.Variant_Parts.Variant_Part
         and Program.Elements.Variant_Parts.Variant_Part_Text
     with private;

   function Create
    (Case_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Discriminant    : not null Program.Elements.Identifiers.Identifier_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Variants        : not null Program.Elements.Variants
         .Variant_Vector_Access;
     End_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Case_Token_2    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Variant_Part;

   type Implicit_Variant_Part is
     new Program.Nodes.Node and Program.Elements.Variant_Parts.Variant_Part
     with private;

   function Create
    (Discriminant         : not null Program.Elements.Identifiers
         .Identifier_Access;
     Variants             : not null Program.Elements.Variants
         .Variant_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Variant_Part
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Variant_Part is
     abstract new Program.Nodes.Node
       and Program.Elements.Variant_Parts.Variant_Part
     with record
        Discriminant : not null Program.Elements.Identifiers.Identifier_Access;
        Variants     : not null Program.Elements.Variants
          .Variant_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Variant_Part'Class);

   overriding procedure Visit
    (Self    : not null access Base_Variant_Part;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Discriminant
    (Self : Base_Variant_Part)
      return not null Program.Elements.Identifiers.Identifier_Access;

   overriding function Variants
    (Self : Base_Variant_Part)
      return not null Program.Elements.Variants.Variant_Vector_Access;

   overriding function Is_Variant_Part
    (Self : Base_Variant_Part)
      return Boolean;

   overriding function Is_Definition (Self : Base_Variant_Part) return Boolean;

   type Variant_Part is
     new Base_Variant_Part and Program.Elements.Variant_Parts.Variant_Part_Text
     with record
        Case_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Case_Token_2    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Variant_Part_Text
    (Self : aliased in out Variant_Part)
      return Program.Elements.Variant_Parts.Variant_Part_Text_Access;

   overriding function Case_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Case_Token_2
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Variant_Part is
     new Base_Variant_Part
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Variant_Part_Text
    (Self : aliased in out Implicit_Variant_Part)
      return Program.Elements.Variant_Parts.Variant_Part_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Variant_Part)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Variant_Part)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Variant_Part)
      return Boolean;

end Program.Nodes.Variant_Parts;
