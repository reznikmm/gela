--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Extension_Aggregates is

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ancestor            : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : not null Program.Elements
         .Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Extension_Aggregate is
   begin
      return Result : Extension_Aggregate :=
        (Left_Bracket_Token => Left_Bracket_Token, Ancestor => Ancestor,
         With_Token => With_Token, Components => Components,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Ancestor             : not null Program.Elements.Expressions
         .Expression_Access;
     Components           : not null Program.Elements
         .Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Extension_Aggregate is
   begin
      return Result : Implicit_Extension_Aggregate :=
        (Ancestor => Ancestor, Components => Components,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Ancestor
    (Self : Base_Extension_Aggregate)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Ancestor;
   end Ancestor;

   overriding function Components
    (Self : Base_Extension_Aggregate)
      return not null Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access is
   begin
      return Self.Components;
   end Components;

   overriding function Left_Bracket_Token
    (Self : Extension_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function With_Token
    (Self : Extension_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Right_Bracket_Token
    (Self : Extension_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Extension_Aggregate)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Extension_Aggregate)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Extension_Aggregate)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Extension_Aggregate'Class) is
   begin
      Set_Enclosing_Element (Self.Ancestor, Self'Unchecked_Access);
      for Item in Self.Components.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Extension_Aggregate
    (Self : Base_Extension_Aggregate)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Extension_Aggregate;

   overriding function Is_Expression
    (Self : Base_Extension_Aggregate)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Extension_Aggregate;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Extension_Aggregate (Self);
   end Visit;

   overriding function To_Extension_Aggregate_Text
    (Self : aliased in out Extension_Aggregate)
      return Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Extension_Aggregate_Text;

   overriding function To_Extension_Aggregate_Text
    (Self : aliased in out Implicit_Extension_Aggregate)
      return Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Extension_Aggregate_Text;

end Program.Nodes.Extension_Aggregates;
