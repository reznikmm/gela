--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Array_Component_Associations is

   function Create
    (Choices         : Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Component_Value : Program.Elements.Expressions.Expression_Access;
     Box_Token       : Program.Lexical_Elements.Lexical_Element_Access)
      return Array_Component_Association is
   begin
      return Result : Array_Component_Association :=
        (Choices => Choices, Arrow_Token => Arrow_Token,
         Component_Value => Component_Value, Box_Token => Box_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Choices              : Program.Element_Vectors.Element_Vector_Access;
     Component_Value      : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Array_Component_Association is
   begin
      return Result : Implicit_Array_Component_Association :=
        (Choices => Choices, Component_Value => Component_Value,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Choices
    (Self : Base_Array_Component_Association)
      return Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Choices;
   end Choices;

   overriding function Component_Value
    (Self : Base_Array_Component_Association)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Component_Value;
   end Component_Value;

   overriding function Arrow_Token
    (Self : Array_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Box_Token
    (Self : Array_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Box_Token;
   end Box_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Array_Component_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Array_Component_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Array_Component_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : in out Base_Array_Component_Association'Class) is
   begin
      for Item in Self.Choices.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.Component_Value.Assigned then
         Set_Enclosing_Element (Self.Component_Value, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Array_Component_Association
    (Self : Base_Array_Component_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Array_Component_Association;

   overriding function Is_Association
    (Self : Base_Array_Component_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Association;

   overriding procedure Visit
    (Self    : not null access Base_Array_Component_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Array_Component_Association (Self);
   end Visit;

   overriding function To_Array_Component_Association_Text
    (Self : in out Array_Component_Association)
      return Program.Elements.Array_Component_Associations
          .Array_Component_Association_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Array_Component_Association_Text;

   overriding function To_Array_Component_Association_Text
    (Self : in out Implicit_Array_Component_Association)
      return Program.Elements.Array_Component_Associations
          .Array_Component_Association_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Array_Component_Association_Text;

end Program.Nodes.Array_Component_Associations;
