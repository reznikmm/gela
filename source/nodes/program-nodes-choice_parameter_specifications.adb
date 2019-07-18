--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Choice_Parameter_Specifications is

   function Create
    (Name        : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Choice_Parameter_Specification is
   begin
      return Result : Choice_Parameter_Specification :=
        (Name => Name, Colon_Token => Colon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Choice_Parameter_Specification is
   begin
      return Result : Implicit_Choice_Parameter_Specification :=
        (Name => Name, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Choice_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Colon_Token
    (Self : Choice_Parameter_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Choice_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Choice_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Choice_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Choice_Parameter_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Choice_Parameter_Specification
    (Self : Base_Choice_Parameter_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Choice_Parameter_Specification;

   overriding function Is_Declaration
    (Self : Base_Choice_Parameter_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Choice_Parameter_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Choice_Parameter_Specification (Self);
   end Visit;

   overriding function To_Choice_Parameter_Specification_Text
    (Self : aliased in out Choice_Parameter_Specification)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Choice_Parameter_Specification_Text;

   overriding function To_Choice_Parameter_Specification_Text
    (Self : aliased in out Implicit_Choice_Parameter_Specification)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Choice_Parameter_Specification_Text;

end Program.Nodes.Choice_Parameter_Specifications;
