--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Explicit_Dereferences is

   function Create
    (Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     All_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Explicit_Dereference is
   begin
      return Result : Explicit_Dereference :=
        (Prefix => Prefix, Dot_Token => Dot_Token, All_Token => All_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Explicit_Dereference is
   begin
      return Result : Implicit_Explicit_Dereference :=
        (Prefix => Prefix, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Prefix
    (Self : Base_Explicit_Dereference)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Prefix;
   end Prefix;

   overriding function Dot_Token
    (Self : Explicit_Dereference)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Dot_Token;
   end Dot_Token;

   overriding function All_Token
    (Self : Explicit_Dereference)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.All_Token;
   end All_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Explicit_Dereference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Explicit_Dereference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Explicit_Dereference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Explicit_Dereference'Class) is
   begin
      Set_Enclosing_Element (Self.Prefix, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Explicit_Dereference_Element
    (Self : Base_Explicit_Dereference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Explicit_Dereference_Element;

   overriding function Is_Expression_Element
    (Self : Base_Explicit_Dereference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Explicit_Dereference;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Explicit_Dereference (Self);
   end Visit;

   overriding function To_Explicit_Dereference_Text
    (Self : aliased in out Explicit_Dereference)
      return Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Explicit_Dereference_Text;

   overriding function To_Explicit_Dereference_Text
    (Self : aliased in out Implicit_Explicit_Dereference)
      return Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Explicit_Dereference_Text;

end Program.Nodes.Explicit_Dereferences;
