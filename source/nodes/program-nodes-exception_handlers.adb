--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Exception_Handlers is

   function Create
    (When_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Choice_Parameter : Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access;
     Choices          : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements       : not null Program.Element_Vectors.Element_Vector_Access)
      return Exception_Handler is
   begin
      return Result : Exception_Handler :=
        (When_Token => When_Token, Choice_Parameter => Choice_Parameter,
         Choices => Choices, Arrow_Token => Arrow_Token,
         Statements => Statements, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Choice_Parameter     : Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Exception_Handler is
   begin
      return Result : Implicit_Exception_Handler :=
        (Choice_Parameter => Choice_Parameter, Choices => Choices,
         Statements => Statements, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Choice_Parameter
    (Self : Base_Exception_Handler)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access is
   begin
      return Self.Choice_Parameter;
   end Choice_Parameter;

   overriding function Choices
    (Self : Base_Exception_Handler)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Choices;
   end Choices;

   overriding function Statements
    (Self : Base_Exception_Handler)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function When_Token
    (Self : Exception_Handler)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.When_Token;
   end When_Token;

   overriding function Arrow_Token
    (Self : Exception_Handler)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Exception_Handler)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Exception_Handler)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Exception_Handler)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Exception_Handler'Class) is
   begin
      if Self.Choice_Parameter.Assigned then
         Set_Enclosing_Element (Self.Choice_Parameter, Self'Unchecked_Access);
      end if;
      for Item in Self.Choices.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Statements.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Exception_Handler_Element
    (Self : Base_Exception_Handler)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Exception_Handler_Element;

   overriding procedure Visit
    (Self    : not null access Base_Exception_Handler;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Exception_Handler (Self);
   end Visit;

   overriding function To_Exception_Handler_Text
    (Self : aliased in out Exception_Handler)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Exception_Handler_Text;

   overriding function To_Exception_Handler_Text
    (Self : aliased in out Implicit_Exception_Handler)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Exception_Handler_Text;

end Program.Nodes.Exception_Handlers;
