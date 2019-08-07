--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Private_Extension_Definitions is

   function Create
    (Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ancestor           : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Private_Extension_Definition is
   begin
      return Result : Private_Extension_Definition :=
        (Abstract_Token => Abstract_Token, Limited_Token => Limited_Token,
         Synchronized_Token => Synchronized_Token, New_Token => New_Token,
         Ancestor => Ancestor, And_Token => And_Token,
         Progenitors => Progenitors, With_Token => With_Token,
         Private_Token => Private_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Ancestor             : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Progenitors          : Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Synchronized     : Boolean := False)
      return Implicit_Private_Extension_Definition is
   begin
      return Result : Implicit_Private_Extension_Definition :=
        (Ancestor => Ancestor, Progenitors => Progenitors,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Abstract => Has_Abstract, Has_Limited => Has_Limited,
         Has_Synchronized => Has_Synchronized, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Ancestor
    (Self : Base_Private_Extension_Definition)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is
   begin
      return Self.Ancestor;
   end Ancestor;

   overriding function Progenitors
    (Self : Base_Private_Extension_Definition)
      return Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Progenitors;
   end Progenitors;

   overriding function Abstract_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abstract_Token;
   end Abstract_Token;

   overriding function Limited_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Limited_Token;
   end Limited_Token;

   overriding function Synchronized_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Synchronized_Token;
   end Synchronized_Token;

   overriding function New_Token
    (Self : Private_Extension_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function And_Token
    (Self : Private_Extension_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.And_Token;
   end And_Token;

   overriding function With_Token
    (Self : Private_Extension_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Private_Token
    (Self : Private_Extension_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Private_Token;
   end Private_Token;

   overriding function Has_Abstract
    (Self : Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Abstract_Token.Assigned;
   end Has_Abstract;

   overriding function Has_Limited
    (Self : Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Limited_Token.Assigned;
   end Has_Limited;

   overriding function Has_Synchronized
    (Self : Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Synchronized_Token.Assigned;
   end Has_Synchronized;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Abstract
    (Self : Implicit_Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Has_Abstract;
   end Has_Abstract;

   overriding function Has_Limited
    (Self : Implicit_Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Has_Limited;
   end Has_Limited;

   overriding function Has_Synchronized
    (Self : Implicit_Private_Extension_Definition)
      return Boolean is
   begin
      return Self.Has_Synchronized;
   end Has_Synchronized;

   procedure Initialize
    (Self : aliased in out Base_Private_Extension_Definition'Class) is
   begin
      Set_Enclosing_Element (Self.Ancestor, Self'Unchecked_Access);
      for Item in Self.Progenitors.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Private_Extension_Definition
    (Self : Base_Private_Extension_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Private_Extension_Definition;

   overriding function Is_Definition
    (Self : Base_Private_Extension_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Private_Extension_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Private_Extension_Definition (Self);
   end Visit;

   overriding function To_Private_Extension_Definition_Text
    (Self : aliased in out Private_Extension_Definition)
      return Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Private_Extension_Definition_Text;

   overriding function To_Private_Extension_Definition_Text
    (Self : aliased in out Implicit_Private_Extension_Definition)
      return Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Private_Extension_Definition_Text;

end Program.Nodes.Private_Extension_Definitions;
