--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Others_Choices is

   function Create
    (Others_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Others_Choice is
   begin
      return Result : Others_Choice :=
        (Others_Token => Others_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Others_Choice is
   begin
      return Result : Implicit_Others_Choice :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Others_Token
    (Self : Others_Choice)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Others_Token;
   end Others_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Others_Choice)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Others_Choice)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Others_Choice)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Others_Choice'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Others_Choice
    (Self : Base_Others_Choice)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Others_Choice;

   overriding function Is_Definition
    (Self : Base_Others_Choice)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Others_Choice;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Others_Choice (Self);
   end Visit;

   overriding function To_Others_Choice_Text
    (Self : aliased in out Others_Choice)
      return Program.Elements.Others_Choices.Others_Choice_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Others_Choice_Text;

   overriding function To_Others_Choice_Text
    (Self : aliased in out Implicit_Others_Choice)
      return Program.Elements.Others_Choices.Others_Choice_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Others_Choice_Text;

end Program.Nodes.Others_Choices;
