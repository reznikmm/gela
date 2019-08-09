--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Loop_Parameter_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Loop_Parameter_Specifications is

   pragma Preelaborate;

   type Loop_Parameter_Specification is
     new Program.Nodes.Node
         and Program.Elements.Loop_Parameter_Specifications
           .Loop_Parameter_Specification
         and Program.Elements.Loop_Parameter_Specifications
           .Loop_Parameter_Specification_Text
     with private;

   function Create
    (Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Definition    : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access)
      return Loop_Parameter_Specification;

   type Implicit_Loop_Parameter_Specification is
     new Program.Nodes.Node
         and Program.Elements.Loop_Parameter_Specifications
           .Loop_Parameter_Specification
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Definition           : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Reverse          : Boolean := False)
      return Implicit_Loop_Parameter_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Loop_Parameter_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification
     with record
        Name       : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Definition : not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Loop_Parameter_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Loop_Parameter_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Loop_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Definition
    (Self : Base_Loop_Parameter_Specification)
      return not null Program.Elements.Discrete_Ranges.Discrete_Range_Access;

   overriding function Is_Loop_Parameter_Specification_Element
    (Self : Base_Loop_Parameter_Specification)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Loop_Parameter_Specification)
      return Boolean;

   type Loop_Parameter_Specification is
     new Base_Loop_Parameter_Specification
       and Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Text
     with record
        In_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Loop_Parameter_Specification_Text
    (Self : aliased in out Loop_Parameter_Specification)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Text_Access;

   overriding function In_Token
    (Self : Loop_Parameter_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Reverse_Token
    (Self : Loop_Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Reverse
    (Self : Loop_Parameter_Specification)
      return Boolean;

   type Implicit_Loop_Parameter_Specification is
     new Base_Loop_Parameter_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Reverse          : Boolean;
     end record;

   overriding function To_Loop_Parameter_Specification_Text
    (Self : aliased in out Implicit_Loop_Parameter_Specification)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean;

   overriding function Has_Reverse
    (Self : Implicit_Loop_Parameter_Specification)
      return Boolean;

end Program.Nodes.Loop_Parameter_Specifications;
