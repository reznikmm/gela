--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Variants;
with Program.Element_Visitors;

package Program.Nodes.Variants is

   pragma Pure (Program.Nodes.Variants);

   type Variant is
     new Program.Nodes.Node and Program.Elements.Variants.Variant
         and Program.Elements.Variants.Variant_Text
     with private;

   function Create
    (When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components  : not null Program.Element_Vectors.Element_Vector_Access)
      return Variant;

   type Implicit_Variant is
     new Program.Nodes.Node and Program.Elements.Variants.Variant
     with private;

   function Create
    (Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Components           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Variant
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Variant is
     abstract new Program.Nodes.Node and Program.Elements.Variants.Variant
     with record
        Choices    : not null Program.Element_Vectors.Element_Vector_Access;
        Components : not null Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Variant'Class);

   overriding procedure Visit
    (Self    : not null access Base_Variant;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Choices
    (Self : Base_Variant)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Components
    (Self : Base_Variant)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Variant (Self : Base_Variant) return Boolean;

   overriding function Is_Definition (Self : Base_Variant) return Boolean;

   type Variant is
     new Base_Variant and Program.Elements.Variants.Variant_Text
     with record
        When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
        Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Variant_Text
    (Self : aliased in out Variant)
      return Program.Elements.Variants.Variant_Text_Access;

   overriding function When_Token
    (Self : Variant)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Arrow_Token
    (Self : Variant)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Variant is
     new Base_Variant
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Variant_Text
    (Self : aliased in out Implicit_Variant)
      return Program.Elements.Variants.Variant_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Variant)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Variant)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Variant)
      return Boolean;

end Program.Nodes.Variants;
