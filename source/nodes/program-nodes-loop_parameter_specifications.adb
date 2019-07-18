--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Loop_Parameter_Specifications is

   function Create
    (Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Definition    : not null Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition_Access)
      return Loop_Parameter_Specification is
   begin
      return Result : Loop_Parameter_Specification :=
        (Name => Name, In_Token => In_Token, Reverse_Token => Reverse_Token,
         Definition => Definition, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Definition           : not null Program.Elements
         .Discrete_Subtype_Definitions.Discrete_Subtype_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Reverse          : Boolean := False)
      return Implicit_Loop_Parameter_Specification is
   begin
      return Result : Implicit_Loop_Parameter_Specification :=
        (Name => Name, Definition => Definition,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Reverse => Has_Reverse, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Loop_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Definition
    (Self : Base_Loop_Parameter_Specification)
      return not null Program.Elements.Discrete_Subtype_Definitions
          .Discrete_Subtype_Definition_Access is
   begin
      return Self.Definition;
   end Definition;

   overriding function In_Token
    (Self : Loop_Parameter_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.In_Token;
   end In_Token;

   overriding function Reverse_Token
    (Self : Loop_Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Reverse_Token;
   end Reverse_Token;

   overriding function Has_Reverse
    (Self : Loop_Parameter_Specification)
      return Boolean is
   begin
      return Self.Reverse_Token.Assigned;
   end Has_Reverse;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Reverse
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean is
   begin
      return Self.Has_Reverse;
   end Has_Reverse;

   procedure Initialize
    (Self : aliased in out Base_Loop_Parameter_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Definition, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Loop_Parameter_Specification
    (Self : Base_Loop_Parameter_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Loop_Parameter_Specification;

   overriding function Is_Declaration
    (Self : Base_Loop_Parameter_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Loop_Parameter_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Loop_Parameter_Specification (Self);
   end Visit;

   overriding function To_Loop_Parameter_Specification_Text
    (Self : aliased in out Loop_Parameter_Specification)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Loop_Parameter_Specification_Text;

   overriding function To_Loop_Parameter_Specification_Text
    (Self : aliased in out Implicit_Loop_Parameter_Specification)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Loop_Parameter_Specification_Text;

end Program.Nodes.Loop_Parameter_Specifications;
