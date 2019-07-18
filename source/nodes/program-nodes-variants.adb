--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Variants is

   function Create
    (When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components  : not null Program.Element_Vectors.Element_Vector_Access)
      return Variant is
   begin
      return Result : Variant :=
        (When_Token => When_Token, Choices => Choices,
         Arrow_Token => Arrow_Token, Components => Components,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Components           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Variant is
   begin
      return Result : Implicit_Variant :=
        (Choices => Choices, Components => Components,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Choices
    (Self : Base_Variant)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Choices;
   end Choices;

   overriding function Components
    (Self : Base_Variant)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Components;
   end Components;

   overriding function When_Token
    (Self : Variant)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.When_Token;
   end When_Token;

   overriding function Arrow_Token
    (Self : Variant)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Variant)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Variant)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Variant)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Variant'Class) is
   begin
      for Item in Self.Choices.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Components.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Variant (Self : Base_Variant) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Variant;

   overriding function Is_Definition (Self : Base_Variant) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Variant;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Variant (Self);
   end Visit;

   overriding function To_Variant_Text
    (Self : aliased in out Variant)
      return Program.Elements.Variants.Variant_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Variant_Text;

   overriding function To_Variant_Text
    (Self : aliased in out Implicit_Variant)
      return Program.Elements.Variants.Variant_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Variant_Text;

end Program.Nodes.Variants;
