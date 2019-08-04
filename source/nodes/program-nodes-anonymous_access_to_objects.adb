--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Anonymous_Access_To_Objects is

   function Create
    (Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access)
      return Anonymous_Access_To_Object is
   begin
      return Result : Anonymous_Access_To_Object :=
        (Not_Token => Not_Token, Null_Token => Null_Token,
         Access_Token => Access_Token, All_Token => All_Token,
         Constant_Token => Constant_Token,
         Subtype_Indication => Subtype_Indication, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_All              : Boolean := False;
     Has_Constant         : Boolean := False)
      return Implicit_Anonymous_Access_To_Object is
   begin
      return Result : Implicit_Anonymous_Access_To_Object :=
        (Subtype_Indication => Subtype_Indication,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Not_Null => Has_Not_Null, Has_All => Has_All,
         Has_Constant => Has_Constant, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Subtype_Indication
    (Self : Base_Anonymous_Access_To_Object)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is
   begin
      return Self.Subtype_Indication;
   end Subtype_Indication;

   overriding function Not_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Access_Token
    (Self : Anonymous_Access_To_Object)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Access_Token;
   end Access_Token;

   overriding function All_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.All_Token;
   end All_Token;

   overriding function Constant_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Constant_Token;
   end Constant_Token;

   overriding function Has_Not_Null
    (Self : Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Has_All
    (Self : Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.All_Token.Assigned;
   end Has_All;

   overriding function Has_Constant
    (Self : Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Constant_Token.Assigned;
   end Has_Constant;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not_Null
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   overriding function Has_All
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Has_All;
   end Has_All;

   overriding function Has_Constant
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean is
   begin
      return Self.Has_Constant;
   end Has_Constant;

   procedure Initialize
    (Self : aliased in out Base_Anonymous_Access_To_Object'Class) is
   begin
      Set_Enclosing_Element (Self.Subtype_Indication, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Anonymous_Access_To_Object
    (Self : Base_Anonymous_Access_To_Object)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Anonymous_Access_To_Object;

   overriding function Is_Anonymous_Access_Definition
    (Self : Base_Anonymous_Access_To_Object)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Anonymous_Access_Definition;

   overriding function Is_Definition
    (Self : Base_Anonymous_Access_To_Object)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Anonymous_Access_To_Object;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Anonymous_Access_To_Object (Self);
   end Visit;

   overriding function To_Anonymous_Access_To_Object_Text
    (Self : aliased in out Anonymous_Access_To_Object)
      return Program.Elements.Anonymous_Access_To_Objects
          .Anonymous_Access_To_Object_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Anonymous_Access_To_Object_Text;

   overriding function To_Anonymous_Access_To_Object_Text
    (Self : aliased in out Implicit_Anonymous_Access_To_Object)
      return Program.Elements.Anonymous_Access_To_Objects
          .Anonymous_Access_To_Object_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Anonymous_Access_To_Object_Text;

end Program.Nodes.Anonymous_Access_To_Objects;
