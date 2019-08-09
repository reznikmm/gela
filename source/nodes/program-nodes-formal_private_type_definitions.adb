--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Private_Type_Definitions is

   function Create
    (Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Tagged_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Private_Type_Definition is
   begin
      return Result : Formal_Private_Type_Definition :=
        (Abstract_Token => Abstract_Token, Tagged_Token => Tagged_Token,
         Limited_Token => Limited_Token, Private_Token => Private_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Tagged           : Boolean := False;
     Has_Limited          : Boolean := False)
      return Implicit_Formal_Private_Type_Definition is
   begin
      return Result : Implicit_Formal_Private_Type_Definition :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Abstract => Has_Abstract, Has_Tagged => Has_Tagged,
         Has_Limited => Has_Limited, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Abstract_Token
    (Self : Formal_Private_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abstract_Token;
   end Abstract_Token;

   overriding function Tagged_Token
    (Self : Formal_Private_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Tagged_Token;
   end Tagged_Token;

   overriding function Limited_Token
    (Self : Formal_Private_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Limited_Token;
   end Limited_Token;

   overriding function Private_Token
    (Self : Formal_Private_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Private_Token;
   end Private_Token;

   overriding function Has_Abstract
    (Self : Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Abstract_Token.Assigned;
   end Has_Abstract;

   overriding function Has_Tagged
    (Self : Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Tagged_Token.Assigned;
   end Has_Tagged;

   overriding function Has_Limited
    (Self : Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Limited_Token.Assigned;
   end Has_Limited;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Abstract
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Has_Abstract;
   end Has_Abstract;

   overriding function Has_Tagged
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Has_Tagged;
   end Has_Tagged;

   overriding function Has_Limited
    (Self : Implicit_Formal_Private_Type_Definition)
      return Boolean is
   begin
      return Self.Has_Limited;
   end Has_Limited;

   procedure Initialize
    (Self : aliased in out Base_Formal_Private_Type_Definition'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Formal_Private_Type_Definition_Element
    (Self : Base_Formal_Private_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Private_Type_Definition_Element;

   overriding function Is_Formal_Type_Definition_Element
    (Self : Base_Formal_Private_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Formal_Private_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Private_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Private_Type_Definition (Self);
   end Visit;

   overriding function To_Formal_Private_Type_Definition_Text
    (Self : aliased in out Formal_Private_Type_Definition)
      return Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Private_Type_Definition_Text;

   overriding function To_Formal_Private_Type_Definition_Text
    (Self : aliased in out Implicit_Formal_Private_Type_Definition)
      return Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Private_Type_Definition_Text;

end Program.Nodes.Formal_Private_Type_Definitions;
