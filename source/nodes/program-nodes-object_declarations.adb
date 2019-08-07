--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Object_Declarations is

   function Create
    (Names                     : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token             : Program.Lexical_Elements
         .Lexical_Element_Access;
     Constant_Token            : Program.Lexical_Elements
         .Lexical_Element_Access;
     Object_Subtype            : not null Program.Elements.Definitions
         .Definition_Access;
     Assignment_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Initialization_Expression : Program.Elements.Expressions
         .Expression_Access;
     With_Token                : Program.Lexical_Elements
         .Lexical_Element_Access;
     Aspects                   : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Object_Declaration is
   begin
      return Result : Object_Declaration :=
        (Names => Names, Colon_Token => Colon_Token,
         Aliased_Token => Aliased_Token, Constant_Token => Constant_Token,
         Object_Subtype => Object_Subtype,
         Assignment_Token => Assignment_Token,
         Initialization_Expression => Initialization_Expression,
         With_Token => With_Token, Aspects => Aspects,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Names                     : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype            : not null Program.Elements.Definitions
         .Definition_Access;
     Initialization_Expression : Program.Elements.Expressions
         .Expression_Access;
     Aspects                   : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit       : Boolean := False;
     Is_Part_Of_Inherited      : Boolean := False;
     Is_Part_Of_Instance       : Boolean := False;
     Has_Aliased               : Boolean := False;
     Has_Constant              : Boolean := False)
      return Implicit_Object_Declaration is
   begin
      return Result : Implicit_Object_Declaration :=
        (Names => Names, Object_Subtype => Object_Subtype,
         Initialization_Expression => Initialization_Expression,
         Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Aliased => Has_Aliased, Has_Constant => Has_Constant,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Object_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Object_Subtype
    (Self : Base_Object_Declaration)
      return not null Program.Elements.Definitions.Definition_Access is
   begin
      return Self.Object_Subtype;
   end Object_Subtype;

   overriding function Initialization_Expression
    (Self : Base_Object_Declaration)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Initialization_Expression;
   end Initialization_Expression;

   overriding function Aspects
    (Self : Base_Object_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Colon_Token
    (Self : Object_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Aliased_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Aliased_Token;
   end Aliased_Token;

   overriding function Constant_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Constant_Token;
   end Constant_Token;

   overriding function Assignment_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function With_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Object_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_Aliased
    (Self : Object_Declaration)
      return Boolean is
   begin
      return Self.Aliased_Token.Assigned;
   end Has_Aliased;

   overriding function Has_Constant
    (Self : Object_Declaration)
      return Boolean is
   begin
      return Self.Constant_Token.Assigned;
   end Has_Constant;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Object_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Object_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Object_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Aliased
    (Self : Implicit_Object_Declaration)
      return Boolean is
   begin
      return Self.Has_Aliased;
   end Has_Aliased;

   overriding function Has_Constant
    (Self : Implicit_Object_Declaration)
      return Boolean is
   begin
      return Self.Has_Constant;
   end Has_Constant;

   procedure Initialize
    (Self : aliased in out Base_Object_Declaration'Class) is
   begin
      for Item in Self.Names.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Object_Subtype, Self'Unchecked_Access);
      if Self.Initialization_Expression.Assigned then
         Set_Enclosing_Element
           (Self.Initialization_Expression, Self'Unchecked_Access);
      end if;
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Object_Declaration
    (Self : Base_Object_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Object_Declaration;

   overriding function Is_Declaration
    (Self : Base_Object_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Object_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Object_Declaration (Self);
   end Visit;

   overriding function To_Object_Declaration_Text
    (Self : aliased in out Object_Declaration)
      return Program.Elements.Object_Declarations
          .Object_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Object_Declaration_Text;

   overriding function To_Object_Declaration_Text
    (Self : aliased in out Implicit_Object_Declaration)
      return Program.Elements.Object_Declarations
          .Object_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Object_Declaration_Text;

end Program.Nodes.Object_Declarations;
