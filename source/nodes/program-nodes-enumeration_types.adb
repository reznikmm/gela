--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Enumeration_Types is

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Literals            : not null Program.Elements
         .Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Enumeration_Type is
   begin
      return Result : Enumeration_Type :=
        (Left_Bracket_Token => Left_Bracket_Token, Literals => Literals,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Literals             : not null Program.Elements
         .Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Enumeration_Type is
   begin
      return Result : Implicit_Enumeration_Type :=
        (Literals => Literals, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Literals
    (Self : Base_Enumeration_Type)
      return not null Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Vector_Access is
   begin
      return Self.Literals;
   end Literals;

   overriding function Left_Bracket_Token
    (Self : Enumeration_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Enumeration_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Enumeration_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Enumeration_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Enumeration_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Enumeration_Type'Class) is
   begin
      for Item in Self.Literals.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Enumeration_Type_Element
    (Self : Base_Enumeration_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Enumeration_Type_Element;

   overriding function Is_Type_Definition_Element
    (Self : Base_Enumeration_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Enumeration_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Enumeration_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Enumeration_Type (Self);
   end Visit;

   overriding function To_Enumeration_Type_Text
    (Self : aliased in out Enumeration_Type)
      return Program.Elements.Enumeration_Types.Enumeration_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Enumeration_Type_Text;

   overriding function To_Enumeration_Type_Text
    (Self : aliased in out Implicit_Enumeration_Type)
      return Program.Elements.Enumeration_Types.Enumeration_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Enumeration_Type_Text;

end Program.Nodes.Enumeration_Types;
