--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Record_Definitions is

   function Create
    (Record_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components     : not null Program.Element_Vectors.Element_Vector_Access;
     End_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Record_Token_2 : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Record_Definition is
   begin
      return Result : Record_Definition :=
        (Record_Token => Record_Token, Components => Components,
         End_Token => End_Token, Record_Token_2 => Record_Token_2,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Components           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Definition is
   begin
      return Result : Implicit_Record_Definition :=
        (Components => Components, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Components
    (Self : Base_Record_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Components;
   end Components;

   overriding function Record_Token
    (Self : Record_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Record_Token;
   end Record_Token;

   overriding function End_Token
    (Self : Record_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Record_Token_2
    (Self : Record_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Record_Token_2;
   end Record_Token_2;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Record_Definition'Class) is
   begin
      for Item in Self.Components.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Record_Definition
    (Self : Base_Record_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Record_Definition;

   overriding function Is_Definition
    (Self : Base_Record_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Record_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Record_Definition (Self);
   end Visit;

   overriding function To_Record_Definition_Text
    (Self : aliased in out Record_Definition)
      return Program.Elements.Record_Definitions
          .Record_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Record_Definition_Text;

   overriding function To_Record_Definition_Text
    (Self : aliased in out Implicit_Record_Definition)
      return Program.Elements.Record_Definitions
          .Record_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Record_Definition_Text;

end Program.Nodes.Record_Definitions;
