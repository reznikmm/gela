--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Object_Declarations is

   function Create
    (Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     In_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Out_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Formal_Object_Declaration is
   begin
      return Result : Formal_Object_Declaration :=
        (Names => Names, Colon_Token => Colon_Token, In_Token => In_Token,
         Out_Token => Out_Token, Not_Token => Not_Token,
         Null_Token => Null_Token, Object_Subtype => Object_Subtype,
         Assignment_Token => Assignment_Token,
         Default_Expression => Default_Expression, With_Token => With_Token,
         Aspects => Aspects, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_In               : Boolean := False;
     Has_Out              : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Formal_Object_Declaration is
   begin
      return Result : Implicit_Formal_Object_Declaration :=
        (Names => Names, Object_Subtype => Object_Subtype,
         Default_Expression => Default_Expression, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Has_In => Has_In,
         Has_Out => Has_Out, Has_Not_Null => Has_Not_Null,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Formal_Object_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Object_Subtype
    (Self : Base_Formal_Object_Declaration)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Object_Subtype;
   end Object_Subtype;

   overriding function Default_Expression
    (Self : Base_Formal_Object_Declaration)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Default_Expression;
   end Default_Expression;

   overriding function Aspects
    (Self : Base_Formal_Object_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Colon_Token
    (Self : Formal_Object_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function In_Token
    (Self : Formal_Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.In_Token;
   end In_Token;

   overriding function Out_Token
    (Self : Formal_Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Out_Token;
   end Out_Token;

   overriding function Not_Token
    (Self : Formal_Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Formal_Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Assignment_Token
    (Self : Formal_Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function With_Token
    (Self : Formal_Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Formal_Object_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_In
    (Self : Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.In_Token.Assigned;
   end Has_In;

   overriding function Has_Out
    (Self : Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Out_Token.Assigned;
   end Has_Out;

   overriding function Has_Not_Null
    (Self : Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_In
    (Self : Implicit_Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Has_In;
   end Has_In;

   overriding function Has_Out
    (Self : Implicit_Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Has_Out;
   end Has_Out;

   overriding function Has_Not_Null
    (Self : Implicit_Formal_Object_Declaration)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   procedure Initialize
    (Self : aliased in out Base_Formal_Object_Declaration'Class) is
   begin
      for Item in Self.Names.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Object_Subtype, Self'Unchecked_Access);
      if Self.Default_Expression.Assigned then
         Set_Enclosing_Element
           (Self.Default_Expression, Self'Unchecked_Access);
      end if;
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Formal_Object_Declaration
    (Self : Base_Formal_Object_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Object_Declaration;

   overriding function Is_Declaration
    (Self : Base_Formal_Object_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Object_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Object_Declaration (Self);
   end Visit;

   overriding function To_Formal_Object_Declaration_Text
    (Self : aliased in out Formal_Object_Declaration)
      return Program.Elements.Formal_Object_Declarations
          .Formal_Object_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Object_Declaration_Text;

   overriding function To_Formal_Object_Declaration_Text
    (Self : aliased in out Implicit_Formal_Object_Declaration)
      return Program.Elements.Formal_Object_Declarations
          .Formal_Object_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Object_Declaration_Text;

end Program.Nodes.Formal_Object_Declarations;
