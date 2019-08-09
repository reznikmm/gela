--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Select_Paths is

   function Create
    (When_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Guard       : Program.Elements.Expressions.Expression_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return Select_Path is
   begin
      return Result : Select_Path :=
        (When_Token => When_Token, Guard => Guard, Arrow_Token => Arrow_Token,
         Statements => Statements, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Guard                : Program.Elements.Expressions.Expression_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Select_Path is
   begin
      return Result : Implicit_Select_Path :=
        (Guard => Guard, Statements => Statements,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Guard
    (Self : Base_Select_Path)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Guard;
   end Guard;

   overriding function Statements
    (Self : Base_Select_Path)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function When_Token
    (Self : Select_Path)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.When_Token;
   end When_Token;

   overriding function Arrow_Token
    (Self : Select_Path)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Select_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Select_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Select_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Select_Path'Class) is
   begin
      if Self.Guard.Assigned then
         Set_Enclosing_Element (Self.Guard, Self'Unchecked_Access);
      end if;
      for Item in Self.Statements.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Select_Path_Element
    (Self : Base_Select_Path)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Select_Path_Element;

   overriding function Is_Path_Element
    (Self : Base_Select_Path)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Path_Element;

   overriding procedure Visit
    (Self    : not null access Base_Select_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Select_Path (Self);
   end Visit;

   overriding function To_Select_Path_Text
    (Self : aliased in out Select_Path)
      return Program.Elements.Select_Paths.Select_Path_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Select_Path_Text;

   overriding function To_Select_Path_Text
    (Self : aliased in out Implicit_Select_Path)
      return Program.Elements.Select_Paths.Select_Path_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Select_Path_Text;

end Program.Nodes.Select_Paths;
