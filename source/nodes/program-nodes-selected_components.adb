--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Selected_Components is

   function Create
    (Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Expressions.Expression_Access)
      return Selected_Component is
   begin
      return Result : Selected_Component :=
        (Prefix => Prefix, Dot_Token => Dot_Token, Selector => Selector,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Selector             : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Selected_Component is
   begin
      return Result : Implicit_Selected_Component :=
        (Prefix => Prefix, Selector => Selector,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Prefix
    (Self : Base_Selected_Component)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Prefix;
   end Prefix;

   overriding function Selector
    (Self : Base_Selected_Component)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Selector;
   end Selector;

   overriding function Dot_Token
    (Self : Selected_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Dot_Token;
   end Dot_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Selected_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Selected_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Selected_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Selected_Component'Class) is
   begin
      Set_Enclosing_Element (Self.Prefix, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Selector, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Selected_Component_Element
    (Self : Base_Selected_Component)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Selected_Component_Element;

   overriding function Is_Expression_Element
    (Self : Base_Selected_Component)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Selected_Component;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Selected_Component (Self);
   end Visit;

   overriding function To_Selected_Component_Text
    (Self : aliased in out Selected_Component)
      return Program.Elements.Selected_Components
          .Selected_Component_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Selected_Component_Text;

   overriding function To_Selected_Component_Text
    (Self : aliased in out Implicit_Selected_Component)
      return Program.Elements.Selected_Components
          .Selected_Component_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Selected_Component_Text;

end Program.Nodes.Selected_Components;
