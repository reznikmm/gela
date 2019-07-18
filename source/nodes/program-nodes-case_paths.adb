--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Case_Paths is

   function Create
    (When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return Case_Path is
   begin
      return Result : Case_Path :=
        (When_Token => When_Token, Choices => Choices,
         Arrow_Token => Arrow_Token, Statements => Statements,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Path is
   begin
      return Result : Implicit_Case_Path :=
        (Choices => Choices, Statements => Statements,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Choices
    (Self : Base_Case_Path)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Choices;
   end Choices;

   overriding function Statements
    (Self : Base_Case_Path)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function When_Token
    (Self : Case_Path)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.When_Token;
   end When_Token;

   overriding function Arrow_Token
    (Self : Case_Path)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Path)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Case_Path'Class) is
   begin
      for Item in Self.Choices.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Case_Path (Self : Base_Case_Path) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Case_Path;

   overriding function Is_Path (Self : Base_Case_Path) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Path;

   overriding procedure Visit
    (Self    : not null access Base_Case_Path;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Case_Path (Self);
   end Visit;

   overriding function To_Case_Path_Text
    (Self : aliased in out Case_Path)
      return Program.Elements.Case_Paths.Case_Path_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Case_Path_Text;

   overriding function To_Case_Path_Text
    (Self : aliased in out Implicit_Case_Path)
      return Program.Elements.Case_Paths.Case_Path_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Case_Path_Text;

end Program.Nodes.Case_Paths;
