--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Discriminant_Associations is

   function Create
    (Selector_Names     : Program.Elements.Identifiers
         .Identifier_Vector_Access;
     Arrow_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Discriminant_Value : not null Program.Elements.Expressions
         .Expression_Access)
      return Discriminant_Association is
   begin
      return Result : Discriminant_Association :=
        (Selector_Names => Selector_Names, Arrow_Token => Arrow_Token,
         Discriminant_Value => Discriminant_Value, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Selector_Names       : Program.Elements.Identifiers
         .Identifier_Vector_Access;
     Discriminant_Value   : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Discriminant_Association is
   begin
      return Result : Implicit_Discriminant_Association :=
        (Selector_Names => Selector_Names,
         Discriminant_Value => Discriminant_Value,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Selector_Names
    (Self : Base_Discriminant_Association)
      return Program.Elements.Identifiers.Identifier_Vector_Access is
   begin
      return Self.Selector_Names;
   end Selector_Names;

   overriding function Discriminant_Value
    (Self : Base_Discriminant_Association)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Discriminant_Value;
   end Discriminant_Value;

   overriding function Arrow_Token
    (Self : Discriminant_Association)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discriminant_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discriminant_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discriminant_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : in out Base_Discriminant_Association'Class) is
   begin
      for Item in Self.Selector_Names.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Discriminant_Value, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Discriminant_Association
    (Self : Base_Discriminant_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discriminant_Association;

   overriding function Is_Association
    (Self : Base_Discriminant_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Association;

   overriding procedure Visit
    (Self    : not null access Base_Discriminant_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Discriminant_Association (Self);
   end Visit;

   overriding function To_Discriminant_Association_Text
    (Self : in out Discriminant_Association)
      return Program.Elements.Discriminant_Associations
          .Discriminant_Association_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Discriminant_Association_Text;

   overriding function To_Discriminant_Association_Text
    (Self : in out Implicit_Discriminant_Association)
      return Program.Elements.Discriminant_Associations
          .Discriminant_Association_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Discriminant_Association_Text;

end Program.Nodes.Discriminant_Associations;
