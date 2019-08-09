--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Null_Components is

   function Create
    (Null_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Null_Component is
   begin
      return Result : Null_Component :=
        (Null_Token => Null_Token, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Null_Component is
   begin
      return Result : Implicit_Null_Component :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Null_Token
    (Self : Null_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Semicolon_Token
    (Self : Null_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Null_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Null_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Null_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Null_Component'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Null_Component_Element
    (Self : Base_Null_Component)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Null_Component_Element;

   overriding function Is_Definition_Element
    (Self : Base_Null_Component)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Null_Component;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Null_Component (Self);
   end Visit;

   overriding function To_Null_Component_Text
    (Self : aliased in out Null_Component)
      return Program.Elements.Null_Components.Null_Component_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Null_Component_Text;

   overriding function To_Null_Component_Text
    (Self : aliased in out Implicit_Null_Component)
      return Program.Elements.Null_Components.Null_Component_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Null_Component_Text;

end Program.Nodes.Null_Components;
