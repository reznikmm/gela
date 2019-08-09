--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Variant_Parts is

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
      return Variant_Part is
   begin
      return Result : Variant_Part :=
        (Case_Token => Case_Token, Discriminant => Discriminant,
         Is_Token => Is_Token, Variants => Variants, End_Token => End_Token,
         Case_Token_2 => Case_Token_2, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Discriminant         : not null Program.Elements.Identifiers
         .Identifier_Access;
     Variants             : not null Program.Elements.Variants
         .Variant_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Variant_Part is
   begin
      return Result : Implicit_Variant_Part :=
        (Discriminant => Discriminant, Variants => Variants,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Discriminant
    (Self : Base_Variant_Part)
      return not null Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.Discriminant;
   end Discriminant;

   overriding function Variants
    (Self : Base_Variant_Part)
      return not null Program.Elements.Variants.Variant_Vector_Access is
   begin
      return Self.Variants;
   end Variants;

   overriding function Case_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Case_Token;
   end Case_Token;

   overriding function Is_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function End_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Case_Token_2
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Case_Token_2;
   end Case_Token_2;

   overriding function Semicolon_Token
    (Self : Variant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Variant_Part)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Variant_Part)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Variant_Part)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Variant_Part'Class) is
   begin
      Set_Enclosing_Element (Self.Discriminant, Self'Unchecked_Access);
      for Item in Self.Variants.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Variant_Part_Element
    (Self : Base_Variant_Part)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Variant_Part_Element;

   overriding function Is_Definition_Element
    (Self : Base_Variant_Part)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Variant_Part;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Variant_Part (Self);
   end Visit;

   overriding function To_Variant_Part_Text
    (Self : aliased in out Variant_Part)
      return Program.Elements.Variant_Parts.Variant_Part_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Variant_Part_Text;

   overriding function To_Variant_Part_Text
    (Self : aliased in out Implicit_Variant_Part)
      return Program.Elements.Variant_Parts.Variant_Part_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Variant_Part_Text;

end Program.Nodes.Variant_Parts;
