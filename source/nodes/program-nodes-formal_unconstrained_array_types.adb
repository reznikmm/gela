--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Unconstrained_Array_Types is

   function Create
    (Array_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Index_Subtypes       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Of_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access)
      return Formal_Unconstrained_Array_Type is
   begin
      return Result : Formal_Unconstrained_Array_Type :=
        (Array_Token => Array_Token, Left_Bracket_Token => Left_Bracket_Token,
         Index_Subtypes => Index_Subtypes,
         Right_Bracket_Token => Right_Bracket_Token, Of_Token => Of_Token,
         Component_Definition => Component_Definition,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Index_Subtypes       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Unconstrained_Array_Type is
   begin
      return Result : Implicit_Formal_Unconstrained_Array_Type :=
        (Index_Subtypes => Index_Subtypes,
         Component_Definition => Component_Definition,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Index_Subtypes
    (Self : Base_Formal_Unconstrained_Array_Type)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Index_Subtypes;
   end Index_Subtypes;

   overriding function Component_Definition
    (Self : Base_Formal_Unconstrained_Array_Type)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access is
   begin
      return Self.Component_Definition;
   end Component_Definition;

   overriding function Array_Token
    (Self : Formal_Unconstrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Array_Token;
   end Array_Token;

   overriding function Left_Bracket_Token
    (Self : Formal_Unconstrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Formal_Unconstrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Of_Token
    (Self : Formal_Unconstrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Of_Token;
   end Of_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Unconstrained_Array_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Unconstrained_Array_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Unconstrained_Array_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Formal_Unconstrained_Array_Type'Class) is
   begin
      for Item in Self.Index_Subtypes.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Component_Definition, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Formal_Unconstrained_Array_Type
    (Self : Base_Formal_Unconstrained_Array_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Unconstrained_Array_Type;

   overriding function Is_Formal_Type_Definition
    (Self : Base_Formal_Unconstrained_Array_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Type_Definition;

   overriding function Is_Definition
    (Self : Base_Formal_Unconstrained_Array_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Unconstrained_Array_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Unconstrained_Array_Type (Self);
   end Visit;

   overriding function To_Formal_Unconstrained_Array_Type_Text
    (Self : aliased in out Formal_Unconstrained_Array_Type)
      return Program.Elements.Formal_Unconstrained_Array_Types
          .Formal_Unconstrained_Array_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Unconstrained_Array_Type_Text;

   overriding function To_Formal_Unconstrained_Array_Type_Text
    (Self : aliased in out Implicit_Formal_Unconstrained_Array_Type)
      return Program.Elements.Formal_Unconstrained_Array_Types
          .Formal_Unconstrained_Array_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Unconstrained_Array_Type_Text;

end Program.Nodes.Formal_Unconstrained_Array_Types;
