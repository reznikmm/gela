--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Floating_Point_Definitions is

   function Create
    (Digits_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token    : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Floating_Point_Definition is
   begin
      return Result : Formal_Floating_Point_Definition :=
        (Digits_Token => Digits_Token, Box_Token => Box_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Floating_Point_Definition is
   begin
      return Result : Implicit_Formal_Floating_Point_Definition :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Digits_Token
    (Self : Formal_Floating_Point_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Digits_Token;
   end Digits_Token;

   overriding function Box_Token
    (Self : Formal_Floating_Point_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Box_Token;
   end Box_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Floating_Point_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Floating_Point_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Floating_Point_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Formal_Floating_Point_Definition'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Formal_Floating_Point_Definition_Element
    (Self : Base_Formal_Floating_Point_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Floating_Point_Definition_Element;

   overriding function Is_Formal_Type_Definition_Element
    (Self : Base_Formal_Floating_Point_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Formal_Floating_Point_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Floating_Point_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Floating_Point_Definition (Self);
   end Visit;

   overriding function To_Formal_Floating_Point_Definition_Text
    (Self : aliased in out Formal_Floating_Point_Definition)
      return Program.Elements.Formal_Floating_Point_Definitions
          .Formal_Floating_Point_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Floating_Point_Definition_Text;

   overriding function To_Formal_Floating_Point_Definition_Text
    (Self : aliased in out Implicit_Formal_Floating_Point_Definition)
      return Program.Elements.Formal_Floating_Point_Definitions
          .Formal_Floating_Point_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Floating_Point_Definition_Text;

end Program.Nodes.Formal_Floating_Point_Definitions;
