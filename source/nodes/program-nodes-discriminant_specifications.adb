--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Discriminant_Specifications is

   function Create
    (Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Discriminant_Specification is
   begin
      return Result : Discriminant_Specification :=
        (Names => Names, Colon_Token => Colon_Token, Not_Token => Not_Token,
         Null_Token => Null_Token, Object_Subtype => Object_Subtype,
         Assignment_Token => Assignment_Token,
         Default_Expression => Default_Expression,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Discriminant_Specification is
   begin
      return Result : Implicit_Discriminant_Specification :=
        (Names => Names, Object_Subtype => Object_Subtype,
         Default_Expression => Default_Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Not_Null => Has_Not_Null, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Discriminant_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Object_Subtype
    (Self : Base_Discriminant_Specification)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Object_Subtype;
   end Object_Subtype;

   overriding function Default_Expression
    (Self : Base_Discriminant_Specification)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Default_Expression;
   end Default_Expression;

   overriding function Colon_Token
    (Self : Discriminant_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Not_Token
    (Self : Discriminant_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Discriminant_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Assignment_Token
    (Self : Discriminant_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function Semicolon_Token
    (Self : Discriminant_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_Not_Null
    (Self : Discriminant_Specification)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discriminant_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discriminant_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discriminant_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not_Null
    (Self : Implicit_Discriminant_Specification)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   procedure Initialize
    (Self : aliased in out Base_Discriminant_Specification'Class) is
   begin
      for Item in Self.Names.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Object_Subtype, Self'Unchecked_Access);
      if Self.Default_Expression.Assigned then
         Set_Enclosing_Element
           (Self.Default_Expression, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Discriminant_Specification
    (Self : Base_Discriminant_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discriminant_Specification;

   overriding function Is_Declaration
    (Self : Base_Discriminant_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Discriminant_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Discriminant_Specification (Self);
   end Visit;

   overriding function To_Discriminant_Specification_Text
    (Self : aliased in out Discriminant_Specification)
      return Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Discriminant_Specification_Text;

   overriding function To_Discriminant_Specification_Text
    (Self : aliased in out Implicit_Discriminant_Specification)
      return Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Discriminant_Specification_Text;

end Program.Nodes.Discriminant_Specifications;
