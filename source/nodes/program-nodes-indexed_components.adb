--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Indexed_Components is

   function Create
    (Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expressions         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Indexed_Component is
   begin
      return Result : Indexed_Component :=
        (Prefix => Prefix, Left_Bracket_Token => Left_Bracket_Token,
         Expressions => Expressions,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Expressions          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Indexed_Component is
   begin
      return Result : Implicit_Indexed_Component :=
        (Prefix => Prefix, Expressions => Expressions,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Prefix
    (Self : Base_Indexed_Component)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Prefix;
   end Prefix;

   overriding function Expressions
    (Self : Base_Indexed_Component)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Expressions;
   end Expressions;

   overriding function Left_Bracket_Token
    (Self : Indexed_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Indexed_Component)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Indexed_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Indexed_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Indexed_Component)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Indexed_Component'Class) is
   begin
      Set_Enclosing_Element (Self.Prefix, Self'Unchecked_Access);
      for Item in Self.Expressions.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Indexed_Component
    (Self : Base_Indexed_Component)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Indexed_Component;

   overriding function Is_Expression
    (Self : Base_Indexed_Component)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Indexed_Component;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Indexed_Component (Self);
   end Visit;

   overriding function To_Indexed_Component_Text
    (Self : aliased in out Indexed_Component)
      return Program.Elements.Indexed_Components
          .Indexed_Component_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Indexed_Component_Text;

   overriding function To_Indexed_Component_Text
    (Self : aliased in out Implicit_Indexed_Component)
      return Program.Elements.Indexed_Components
          .Indexed_Component_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Indexed_Component_Text;

end Program.Nodes.Indexed_Components;
