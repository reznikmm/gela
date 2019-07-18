--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Object_Renaming_Declarations is

   function Create
    (Names           : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype  : not null Program.Elements.Element_Access;
     Renames_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Object  : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Object_Renaming_Declaration is
   begin
      return Result : Object_Renaming_Declaration :=
        (Names => Names, Colon_Token => Colon_Token, Not_Token => Not_Token,
         Null_Token => Null_Token, Object_Subtype => Object_Subtype,
         Renames_Token => Renames_Token, Renamed_Object => Renamed_Object,
         With_Token => With_Token, Aspects => Aspects,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Renamed_Object       : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Object_Renaming_Declaration is
   begin
      return Result : Implicit_Object_Renaming_Declaration :=
        (Names => Names, Object_Subtype => Object_Subtype,
         Renamed_Object => Renamed_Object, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Not_Null => Has_Not_Null, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Object_Renaming_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Object_Subtype
    (Self : Base_Object_Renaming_Declaration)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Object_Subtype;
   end Object_Subtype;

   overriding function Renamed_Object
    (Self : Base_Object_Renaming_Declaration)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Renamed_Object;
   end Renamed_Object;

   overriding function Aspects
    (Self : Base_Object_Renaming_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Colon_Token
    (Self : Object_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Not_Token
    (Self : Object_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Object_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Renames_Token
    (Self : Object_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Renames_Token;
   end Renames_Token;

   overriding function With_Token
    (Self : Object_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Object_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_Not_Null
    (Self : Object_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not_Null
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   procedure Initialize
    (Self : aliased in out Base_Object_Renaming_Declaration'Class) is
   begin
      for Item in Self.Names.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Object_Subtype, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Renamed_Object, Self'Unchecked_Access);
      for Item in Self.Aspects.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Object_Renaming_Declaration
    (Self : Base_Object_Renaming_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Object_Renaming_Declaration;

   overriding function Is_Declaration
    (Self : Base_Object_Renaming_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Object_Renaming_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Object_Renaming_Declaration (Self);
   end Visit;

   overriding function To_Object_Renaming_Declaration_Text
    (Self : aliased in out Object_Renaming_Declaration)
      return Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Object_Renaming_Declaration_Text;

   overriding function To_Object_Renaming_Declaration_Text
    (Self : aliased in out Implicit_Object_Renaming_Declaration)
      return Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Object_Renaming_Declaration_Text;

end Program.Nodes.Object_Renaming_Declarations;
