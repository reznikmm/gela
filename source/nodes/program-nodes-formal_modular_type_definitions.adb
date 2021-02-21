--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Formal_Modular_Type_Definitions is

   function Create
    (Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Modular_Type_Definition is
   begin
      return Result : Formal_Modular_Type_Definition :=
        (Mod_Token => Mod_Token, Box_Token => Box_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Modular_Type_Definition is
   begin
      return Result : Implicit_Formal_Modular_Type_Definition :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Mod_Token
    (Self : Formal_Modular_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Mod_Token;
   end Mod_Token;

   overriding function Box_Token
    (Self : Formal_Modular_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Box_Token;
   end Box_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Modular_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Modular_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Modular_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : in out Base_Formal_Modular_Type_Definition'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Formal_Modular_Type_Definition
    (Self : Base_Formal_Modular_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Modular_Type_Definition;

   overriding function Is_Formal_Type_Definition
    (Self : Base_Formal_Modular_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Type_Definition;

   overriding function Is_Definition
    (Self : Base_Formal_Modular_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Modular_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Modular_Type_Definition (Self);
   end Visit;

   overriding function To_Formal_Modular_Type_Definition_Text
    (Self : in out Formal_Modular_Type_Definition)
      return Program.Elements.Formal_Modular_Type_Definitions
          .Formal_Modular_Type_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Modular_Type_Definition_Text;

   overriding function To_Formal_Modular_Type_Definition_Text
    (Self : in out Implicit_Formal_Modular_Type_Definition)
      return Program.Elements.Formal_Modular_Type_Definitions
          .Formal_Modular_Type_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Modular_Type_Definition_Text;

end Program.Nodes.Formal_Modular_Type_Definitions;
