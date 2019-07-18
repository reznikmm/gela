--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Derived_Type_Definitions is

   function Create
    (Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Mark       : not null Program.Elements.Expressions
         .Expression_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token      : Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Derived_Type_Definition is
   begin
      return Result : Formal_Derived_Type_Definition :=
        (Abstract_Token => Abstract_Token, Limited_Token => Limited_Token,
         Synchronized_Token => Synchronized_Token, New_Token => New_Token,
         Subtype_Mark => Subtype_Mark, And_Token => And_Token,
         Progenitors => Progenitors, With_Token => With_Token,
         Private_Token => Private_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Synchronized     : Boolean := False;
     Has_With_Private     : Boolean := False)
      return Implicit_Formal_Derived_Type_Definition is
   begin
      return Result : Implicit_Formal_Derived_Type_Definition :=
        (Subtype_Mark => Subtype_Mark, Progenitors => Progenitors,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Abstract => Has_Abstract, Has_Limited => Has_Limited,
         Has_Synchronized => Has_Synchronized,
         Has_With_Private => Has_With_Private, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Subtype_Mark
    (Self : Base_Formal_Derived_Type_Definition)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Subtype_Mark;
   end Subtype_Mark;

   overriding function Progenitors
    (Self : Base_Formal_Derived_Type_Definition)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Progenitors;
   end Progenitors;

   overriding function Abstract_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abstract_Token;
   end Abstract_Token;

   overriding function Limited_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Limited_Token;
   end Limited_Token;

   overriding function Synchronized_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Synchronized_Token;
   end Synchronized_Token;

   overriding function New_Token
    (Self : Formal_Derived_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function And_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.And_Token;
   end And_Token;

   overriding function With_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Private_Token
    (Self : Formal_Derived_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Private_Token;
   end Private_Token;

   overriding function Has_Abstract
    (Self : Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Abstract_Token.Assigned;
   end Has_Abstract;

   overriding function Has_Limited
    (Self : Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Limited_Token.Assigned;
   end Has_Limited;

   overriding function Has_Synchronized
    (Self : Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Synchronized_Token.Assigned;
   end Has_Synchronized;

   overriding function Has_With_Private
    (Self : Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.With_Private_Token.Assigned;
   end Has_With_Private;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Abstract
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Has_Abstract;
   end Has_Abstract;

   overriding function Has_Limited
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Has_Limited;
   end Has_Limited;

   overriding function Has_Synchronized
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Has_Synchronized;
   end Has_Synchronized;

   overriding function Has_With_Private
    (Self : Implicit_Formal_Derived_Type_Definition)
      return Boolean is
   begin
      return Self.Has_With_Private;
   end Has_With_Private;

   procedure Initialize
    (Self : aliased in out Base_Formal_Derived_Type_Definition'Class) is
   begin
      Set_Enclosing_Element (Self.Subtype_Mark, Self'Unchecked_Access);
      for Item in Self.Progenitors.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Formal_Derived_Type_Definition
    (Self : Base_Formal_Derived_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Derived_Type_Definition;

   overriding function Is_Formal_Type_Definition
    (Self : Base_Formal_Derived_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Type_Definition;

   overriding function Is_Definition
    (Self : Base_Formal_Derived_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Derived_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Derived_Type_Definition (Self);
   end Visit;

   overriding function To_Formal_Derived_Type_Definition_Text
    (Self : aliased in out Formal_Derived_Type_Definition)
      return Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Derived_Type_Definition_Text;

   overriding function To_Formal_Derived_Type_Definition_Text
    (Self : aliased in out Implicit_Formal_Derived_Type_Definition)
      return Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Derived_Type_Definition_Text;

end Program.Nodes.Formal_Derived_Type_Definitions;
