--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Allocators is

   function Create
    (New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Subpool_Name         : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication   : Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Qualified_Expression : Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access)
      return Allocator is
   begin
      return Result : Allocator :=
        (New_Token => New_Token, Left_Bracket_Token => Left_Bracket_Token,
         Subpool_Name => Subpool_Name,
         Right_Bracket_Token => Right_Bracket_Token,
         Subtype_Indication => Subtype_Indication,
         Qualified_Expression => Qualified_Expression,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Subpool_Name         : Program.Elements.Expressions.Expression_Access;
     Subtype_Indication   : Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Qualified_Expression : Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Allocator is
   begin
      return Result : Implicit_Allocator :=
        (Subpool_Name => Subpool_Name,
         Subtype_Indication => Subtype_Indication,
         Qualified_Expression => Qualified_Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Subpool_Name
    (Self : Base_Allocator)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Subpool_Name;
   end Subpool_Name;

   overriding function Subtype_Indication
    (Self : Base_Allocator)
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access is
   begin
      return Self.Subtype_Indication;
   end Subtype_Indication;

   overriding function Qualified_Expression
    (Self : Base_Allocator)
      return Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access is
   begin
      return Self.Qualified_Expression;
   end Qualified_Expression;

   overriding function New_Token
    (Self : Allocator)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function Left_Bracket_Token
    (Self : Allocator)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Allocator)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Allocator)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Allocator)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Allocator)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Allocator'Class) is
   begin
      if Self.Subpool_Name.Assigned then
         Set_Enclosing_Element (Self.Subpool_Name, Self'Unchecked_Access);
      end if;
      if Self.Subtype_Indication.Assigned then
         Set_Enclosing_Element
           (Self.Subtype_Indication, Self'Unchecked_Access);
      end if;
      if Self.Qualified_Expression.Assigned then
         Set_Enclosing_Element
           (Self.Qualified_Expression, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Allocator (Self : Base_Allocator) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Allocator;

   overriding function Is_Expression (Self : Base_Allocator) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Allocator;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Allocator (Self);
   end Visit;

   overriding function To_Allocator_Text
    (Self : aliased in out Allocator)
      return Program.Elements.Allocators.Allocator_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Allocator_Text;

   overriding function To_Allocator_Text
    (Self : aliased in out Implicit_Allocator)
      return Program.Elements.Allocators.Allocator_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Allocator_Text;

end Program.Nodes.Allocators;
