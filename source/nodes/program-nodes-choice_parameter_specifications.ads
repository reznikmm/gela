--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Choice_Parameter_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Choice_Parameter_Specifications is

   pragma Pure (Program.Nodes.Choice_Parameter_Specifications);

   type Choice_Parameter_Specification is
     new Program.Nodes.Node
         and Program.Elements.Choice_Parameter_Specifications
           .Choice_Parameter_Specification
         and Program.Elements.Choice_Parameter_Specifications
           .Choice_Parameter_Specification_Text
     with private;

   function Create
    (Name        : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Choice_Parameter_Specification;

   type Implicit_Choice_Parameter_Specification is
     new Program.Nodes.Node
         and Program.Elements.Choice_Parameter_Specifications
           .Choice_Parameter_Specification
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Choice_Parameter_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Choice_Parameter_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification
     with record
        Name : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Choice_Parameter_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Choice_Parameter_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Choice_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Is_Choice_Parameter_Specification
    (Self : Base_Choice_Parameter_Specification)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Choice_Parameter_Specification)
      return Boolean;

   type Choice_Parameter_Specification is
     new Base_Choice_Parameter_Specification
       and Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Text
     with record
        Colon_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Choice_Parameter_Specification_Text
    (Self : aliased in out Choice_Parameter_Specification)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Text_Access;

   overriding function Colon_Token
    (Self : Choice_Parameter_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Choice_Parameter_Specification is
     new Base_Choice_Parameter_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Choice_Parameter_Specification_Text
    (Self : aliased in out Implicit_Choice_Parameter_Specification)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Choice_Parameter_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Choice_Parameter_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Choice_Parameter_Specification)
      return Boolean;

end Program.Nodes.Choice_Parameter_Specifications;
