--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Derived_Record_Extensions is

   function Create
    (Abstract_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Parent            : not null Program.Elements.Expressions
         .Expression_Access;
     And_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : Program.Elements.Expressions.Expression_Vector_Access;
     With_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Definition : not null Program.Elements.Definitions
         .Definition_Access)
      return Derived_Record_Extension is
   begin
      return Result : Derived_Record_Extension :=
        (Abstract_Token => Abstract_Token, Limited_Token => Limited_Token,
         New_Token => New_Token, Parent => Parent, And_Token => And_Token,
         Progenitors => Progenitors, With_Token => With_Token,
         Record_Definition => Record_Definition, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Parent               : not null Program.Elements.Expressions
         .Expression_Access;
     Progenitors          : Program.Elements.Expressions
         .Expression_Vector_Access;
     Record_Definition    : not null Program.Elements.Definitions
         .Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False)
      return Implicit_Derived_Record_Extension is
   begin
      return Result : Implicit_Derived_Record_Extension :=
        (Parent => Parent, Progenitors => Progenitors,
         Record_Definition => Record_Definition,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Abstract => Has_Abstract, Has_Limited => Has_Limited,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Parent
    (Self : Base_Derived_Record_Extension)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Parent;
   end Parent;

   overriding function Progenitors
    (Self : Base_Derived_Record_Extension)
      return Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Progenitors;
   end Progenitors;

   overriding function Record_Definition
    (Self : Base_Derived_Record_Extension)
      return not null Program.Elements.Definitions.Definition_Access is
   begin
      return Self.Record_Definition;
   end Record_Definition;

   overriding function Abstract_Token
    (Self : Derived_Record_Extension)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abstract_Token;
   end Abstract_Token;

   overriding function Limited_Token
    (Self : Derived_Record_Extension)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Limited_Token;
   end Limited_Token;

   overriding function New_Token
    (Self : Derived_Record_Extension)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function And_Token
    (Self : Derived_Record_Extension)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.And_Token;
   end And_Token;

   overriding function With_Token
    (Self : Derived_Record_Extension)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Has_Abstract
    (Self : Derived_Record_Extension)
      return Boolean is
   begin
      return Self.Abstract_Token.Assigned;
   end Has_Abstract;

   overriding function Has_Limited
    (Self : Derived_Record_Extension)
      return Boolean is
   begin
      return Self.Limited_Token.Assigned;
   end Has_Limited;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Derived_Record_Extension)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Derived_Record_Extension)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Derived_Record_Extension)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Abstract
    (Self : Implicit_Derived_Record_Extension)
      return Boolean is
   begin
      return Self.Has_Abstract;
   end Has_Abstract;

   overriding function Has_Limited
    (Self : Implicit_Derived_Record_Extension)
      return Boolean is
   begin
      return Self.Has_Limited;
   end Has_Limited;

   procedure Initialize
    (Self : aliased in out Base_Derived_Record_Extension'Class) is
   begin
      Set_Enclosing_Element (Self.Parent, Self'Unchecked_Access);
      for Item in Self.Progenitors.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Record_Definition, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Derived_Record_Extension_Element
    (Self : Base_Derived_Record_Extension)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Derived_Record_Extension_Element;

   overriding function Is_Type_Definition_Element
    (Self : Base_Derived_Record_Extension)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Derived_Record_Extension)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Derived_Record_Extension;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Derived_Record_Extension (Self);
   end Visit;

   overriding function To_Derived_Record_Extension_Text
    (Self : aliased in out Derived_Record_Extension)
      return Program.Elements.Derived_Record_Extensions
          .Derived_Record_Extension_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Derived_Record_Extension_Text;

   overriding function To_Derived_Record_Extension_Text
    (Self : aliased in out Implicit_Derived_Record_Extension)
      return Program.Elements.Derived_Record_Extensions
          .Derived_Record_Extension_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Derived_Record_Extension_Text;

end Program.Nodes.Derived_Record_Extensions;
