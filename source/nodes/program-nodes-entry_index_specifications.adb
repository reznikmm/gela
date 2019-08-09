--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Entry_Index_Specifications is

   function Create
    (For_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Index_Subtype : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access)
      return Entry_Index_Specification is
   begin
      return Result : Entry_Index_Specification :=
        (For_Token => For_Token, Name => Name, In_Token => In_Token,
         Entry_Index_Subtype => Entry_Index_Subtype, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Entry_Index_Subtype  : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Entry_Index_Specification is
   begin
      return Result : Implicit_Entry_Index_Specification :=
        (Name => Name, Entry_Index_Subtype => Entry_Index_Subtype,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Entry_Index_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Entry_Index_Subtype
    (Self : Base_Entry_Index_Specification)
      return not null Program.Elements.Discrete_Ranges.Discrete_Range_Access is
   begin
      return Self.Entry_Index_Subtype;
   end Entry_Index_Subtype;

   overriding function For_Token
    (Self : Entry_Index_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.For_Token;
   end For_Token;

   overriding function In_Token
    (Self : Entry_Index_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.In_Token;
   end In_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Entry_Index_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Entry_Index_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Entry_Index_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Entry_Index_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Entry_Index_Subtype, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Entry_Index_Specification_Element
    (Self : Base_Entry_Index_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Entry_Index_Specification_Element;

   overriding function Is_Declaration_Element
    (Self : Base_Entry_Index_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration_Element;

   overriding procedure Visit
    (Self    : not null access Base_Entry_Index_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Entry_Index_Specification (Self);
   end Visit;

   overriding function To_Entry_Index_Specification_Text
    (Self : aliased in out Entry_Index_Specification)
      return Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Entry_Index_Specification_Text;

   overriding function To_Entry_Index_Specification_Text
    (Self : aliased in out Implicit_Entry_Index_Specification)
      return Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Entry_Index_Specification_Text;

end Program.Nodes.Entry_Index_Specifications;
