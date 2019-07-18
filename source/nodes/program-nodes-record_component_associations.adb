--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Record_Component_Associations is

   function Create
    (Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return Record_Component_Association is
   begin
      return Result : Record_Component_Association :=
        (Choices => Choices, Arrow_Token => Arrow_Token,
         Expression => Expression, Box_Token => Box_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Component_Association is
   begin
      return Result : Implicit_Record_Component_Association :=
        (Choices => Choices, Expression => Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Choices
    (Self : Base_Record_Component_Association)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Choices;
   end Choices;

   overriding function Expression
    (Self : Base_Record_Component_Association)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Arrow_Token
    (Self : Record_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Box_Token
    (Self : Record_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Box_Token;
   end Box_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Component_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Component_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Component_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Record_Component_Association'Class) is
   begin
      for Item in Self.Choices.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.Expression.Assigned then
         Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Record_Component_Association
    (Self : Base_Record_Component_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Record_Component_Association;

   overriding function Is_Association
    (Self : Base_Record_Component_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Association;

   overriding procedure Visit
    (Self    : not null access Base_Record_Component_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Record_Component_Association (Self);
   end Visit;

   overriding function To_Record_Component_Association_Text
    (Self : aliased in out Record_Component_Association)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Record_Component_Association_Text;

   overriding function To_Record_Component_Association_Text
    (Self : aliased in out Implicit_Record_Component_Association)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Record_Component_Association_Text;

end Program.Nodes.Record_Component_Associations;
