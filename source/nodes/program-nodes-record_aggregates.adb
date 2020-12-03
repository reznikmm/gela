--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Record_Aggregates is

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Record_Aggregate is
   begin
      return Result : Record_Aggregate :=
        (Left_Bracket_Token => Left_Bracket_Token, Components => Components,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Components           : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Aggregate is
   begin
      return Result : Implicit_Record_Aggregate :=
        (Components => Components, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Components
    (Self : Base_Record_Aggregate)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access is
   begin
      return Self.Components;
   end Components;

   overriding function Left_Bracket_Token
    (Self : Record_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Record_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Aggregate)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Aggregate)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Aggregate)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Record_Aggregate'Class) is
   begin
      for Item in Self.Components.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Record_Aggregate
    (Self : Base_Record_Aggregate)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Record_Aggregate;

   overriding function Is_Expression
    (Self : Base_Record_Aggregate)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Record_Aggregate;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Record_Aggregate (Self);
   end Visit;

   overriding function To_Record_Aggregate_Text
    (Self : aliased in out Record_Aggregate)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Record_Aggregate_Text;

   overriding function To_Record_Aggregate_Text
    (Self : aliased in out Implicit_Record_Aggregate)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Record_Aggregate_Text;

end Program.Nodes.Record_Aggregates;
