--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Aspect_Specifications is

   function Create
    (Aspect_Mark       : not null Program.Elements.Expressions
         .Expression_Access;
     Arrow_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aspect_Definition : not null Program.Elements.Expressions
         .Expression_Access)
      return Aspect_Specification is
   begin
      return Result : Aspect_Specification :=
        (Aspect_Mark => Aspect_Mark, Arrow_Token => Arrow_Token,
         Aspect_Definition => Aspect_Definition, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Aspect_Mark          : not null Program.Elements.Expressions
         .Expression_Access;
     Aspect_Definition    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Aspect_Specification is
   begin
      return Result : Implicit_Aspect_Specification :=
        (Aspect_Mark => Aspect_Mark, Aspect_Definition => Aspect_Definition,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Aspect_Mark
    (Self : Base_Aspect_Specification)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Aspect_Mark;
   end Aspect_Mark;

   overriding function Aspect_Definition
    (Self : Base_Aspect_Specification)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Aspect_Definition;
   end Aspect_Definition;

   overriding function Arrow_Token
    (Self : Aspect_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Aspect_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Aspect_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Aspect_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Aspect_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Aspect_Mark, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Aspect_Definition, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Aspect_Specification
    (Self : Base_Aspect_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Aspect_Specification;

   overriding function Is_Definition
    (Self : Base_Aspect_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Aspect_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Aspect_Specification (Self);
   end Visit;

   overriding function To_Aspect_Specification_Text
    (Self : aliased in out Aspect_Specification)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Aspect_Specification_Text;

   overriding function To_Aspect_Specification_Text
    (Self : aliased in out Implicit_Aspect_Specification)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Aspect_Specification_Text;

end Program.Nodes.Aspect_Specifications;
