--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Modular_Types is

   function Create
    (Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Modulus   : not null Program.Elements.Expressions.Expression_Access)
      return Modular_Type is
   begin
      return Result : Modular_Type :=
        (Mod_Token => Mod_Token, Modulus => Modulus, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Modulus              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Modular_Type is
   begin
      return Result : Implicit_Modular_Type :=
        (Modulus => Modulus, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Modulus
    (Self : Base_Modular_Type)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Modulus;
   end Modulus;

   overriding function Mod_Token
    (Self : Modular_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Mod_Token;
   end Mod_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Modular_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Modular_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Modular_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Modular_Type'Class) is
   begin
      Set_Enclosing_Element (Self.Modulus, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Modular_Type
    (Self : Base_Modular_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Modular_Type;

   overriding function Is_Type_Definition
    (Self : Base_Modular_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition;

   overriding function Is_Definition
    (Self : Base_Modular_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Modular_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Modular_Type (Self);
   end Visit;

   overriding function To_Modular_Type_Text
    (Self : aliased in out Modular_Type)
      return Program.Elements.Modular_Types.Modular_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Modular_Type_Text;

   overriding function To_Modular_Type_Text
    (Self : aliased in out Implicit_Modular_Type)
      return Program.Elements.Modular_Types.Modular_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Modular_Type_Text;

end Program.Nodes.Modular_Types;
