--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Component_Declarations is

   function Create
    (Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Component_Declaration is
   begin
      return Result : Component_Declaration :=
        (Names => Names, Colon_Token => Colon_Token,
         Object_Subtype => Object_Subtype,
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
     Object_Subtype       : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Component_Declaration is
   begin
      return Result : Implicit_Component_Declaration :=
        (Names => Names, Object_Subtype => Object_Subtype,
         Default_Expression => Default_Expression, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Component_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Object_Subtype
    (Self : Base_Component_Declaration)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access is
   begin
      return Self.Object_Subtype;
   end Object_Subtype;

   overriding function Default_Expression
    (Self : Base_Component_Declaration)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Default_Expression;
   end Default_Expression;

   overriding function Aspects
    (Self : Base_Component_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Colon_Token
    (Self : Component_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Assignment_Token
    (Self : Component_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function With_Token
    (Self : Component_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Component_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Component_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Component_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Component_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Component_Declaration'Class) is
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

   overriding function Is_Component_Declaration
    (Self : Base_Component_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Component_Declaration;

   overriding function Is_Declaration
    (Self : Base_Component_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Component_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Component_Declaration (Self);
   end Visit;

   overriding function To_Component_Declaration_Text
    (Self : aliased in out Component_Declaration)
      return Program.Elements.Component_Declarations
          .Component_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Component_Declaration_Text;

   overriding function To_Component_Declaration_Text
    (Self : aliased in out Implicit_Component_Declaration)
      return Program.Elements.Component_Declarations
          .Component_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Component_Declaration_Text;

end Program.Nodes.Component_Declarations;
