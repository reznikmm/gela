--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Return_Object_Specifications is

   function Create
    (Name             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype   : not null Program.Elements.Element_Access;
     Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression       : Program.Elements.Expressions.Expression_Access)
      return Return_Object_Specification is
   begin
      return Result : Return_Object_Specification :=
        (Name => Name, Colon_Token => Colon_Token,
         Aliased_Token => Aliased_Token, Constant_Token => Constant_Token,
         Object_Subtype => Object_Subtype,
         Assignment_Token => Assignment_Token, Expression => Expression,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Aliased          : Boolean := False;
     Has_Constant         : Boolean := False)
      return Implicit_Return_Object_Specification is
   begin
      return Result : Implicit_Return_Object_Specification :=
        (Name => Name, Object_Subtype => Object_Subtype,
         Expression => Expression, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Aliased => Has_Aliased, Has_Constant => Has_Constant,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Return_Object_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Object_Subtype
    (Self : Base_Return_Object_Specification)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Object_Subtype;
   end Object_Subtype;

   overriding function Expression
    (Self : Base_Return_Object_Specification)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Colon_Token
    (Self : Return_Object_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Aliased_Token
    (Self : Return_Object_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Aliased_Token;
   end Aliased_Token;

   overriding function Constant_Token
    (Self : Return_Object_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Constant_Token;
   end Constant_Token;

   overriding function Assignment_Token
    (Self : Return_Object_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function Has_Aliased
    (Self : Return_Object_Specification)
      return Boolean is
   begin
      return Self.Aliased_Token.Assigned;
   end Has_Aliased;

   overriding function Has_Constant
    (Self : Return_Object_Specification)
      return Boolean is
   begin
      return Self.Constant_Token.Assigned;
   end Has_Constant;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Return_Object_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Return_Object_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Return_Object_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Aliased
    (Self : Implicit_Return_Object_Specification)
      return Boolean is
   begin
      return Self.Has_Aliased;
   end Has_Aliased;

   overriding function Has_Constant
    (Self : Implicit_Return_Object_Specification)
      return Boolean is
   begin
      return Self.Has_Constant;
   end Has_Constant;

   procedure Initialize
    (Self : aliased in out Base_Return_Object_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Object_Subtype, Self'Unchecked_Access);
      if Self.Expression.Assigned then
         Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Return_Object_Specification
    (Self : Base_Return_Object_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Return_Object_Specification;

   overriding function Is_Declaration
    (Self : Base_Return_Object_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Return_Object_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Return_Object_Specification (Self);
   end Visit;

   overriding function To_Return_Object_Specification_Text
    (Self : aliased in out Return_Object_Specification)
      return Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Return_Object_Specification_Text;

   overriding function To_Return_Object_Specification_Text
    (Self : aliased in out Implicit_Return_Object_Specification)
      return Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Return_Object_Specification_Text;

end Program.Nodes.Return_Object_Specifications;
