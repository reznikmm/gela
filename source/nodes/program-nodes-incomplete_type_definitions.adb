--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Incomplete_Type_Definitions is

   function Create
    (Tagged_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Incomplete_Type_Definition is
   begin
      return Result : Incomplete_Type_Definition :=
        (Tagged_Token => Tagged_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Tagged           : Boolean := False)
      return Implicit_Incomplete_Type_Definition is
   begin
      return Result : Implicit_Incomplete_Type_Definition :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Has_Tagged => Has_Tagged,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Tagged_Token
    (Self : Incomplete_Type_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Tagged_Token;
   end Tagged_Token;

   overriding function Has_Tagged
    (Self : Incomplete_Type_Definition)
      return Boolean is
   begin
      return Self.Tagged_Token.Assigned;
   end Has_Tagged;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Tagged
    (Self : Implicit_Incomplete_Type_Definition)
      return Boolean is
   begin
      return Self.Has_Tagged;
   end Has_Tagged;

   procedure Initialize
    (Self : aliased in out Base_Incomplete_Type_Definition'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Incomplete_Type_Definition_Element
    (Self : Base_Incomplete_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Incomplete_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Incomplete_Type_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Incomplete_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Incomplete_Type_Definition (Self);
   end Visit;

   overriding function To_Incomplete_Type_Definition_Text
    (Self : aliased in out Incomplete_Type_Definition)
      return Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Incomplete_Type_Definition_Text;

   overriding function To_Incomplete_Type_Definition_Text
    (Self : aliased in out Implicit_Incomplete_Type_Definition)
      return Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Incomplete_Type_Definition_Text;

end Program.Nodes.Incomplete_Type_Definitions;
