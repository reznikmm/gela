--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Aspect_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Aspect_Specifications is

   pragma Preelaborate;

   type Aspect_Specification is
     new Program.Nodes.Node
         and Program.Elements.Aspect_Specifications.Aspect_Specification
         and Program.Elements.Aspect_Specifications.Aspect_Specification_Text
     with private;

   function Create
    (Aspect_Mark       : not null Program.Elements.Expressions
         .Expression_Access;
     Arrow_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aspect_Definition : not null Program.Elements.Expressions
         .Expression_Access)
      return Aspect_Specification;

   type Implicit_Aspect_Specification is
     new Program.Nodes.Node
         and Program.Elements.Aspect_Specifications.Aspect_Specification
     with private;

   function Create
    (Aspect_Mark          : not null Program.Elements.Expressions
         .Expression_Access;
     Aspect_Definition    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Aspect_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Aspect_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Aspect_Specifications.Aspect_Specification
     with record
        Aspect_Mark       : not null Program.Elements.Expressions
          .Expression_Access;
        Aspect_Definition : not null Program.Elements.Expressions
          .Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Aspect_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Aspect_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Aspect_Mark
    (Self : Base_Aspect_Specification)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Aspect_Definition
    (Self : Base_Aspect_Specification)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Aspect_Specification_Element
    (Self : Base_Aspect_Specification)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Aspect_Specification)
      return Boolean;

   type Aspect_Specification is
     new Base_Aspect_Specification
       and Program.Elements.Aspect_Specifications.Aspect_Specification_Text
     with record
        Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Aspect_Specification_Text
    (Self : aliased in out Aspect_Specification)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Text_Access;

   overriding function Arrow_Token
    (Self : Aspect_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Aspect_Specification is
     new Base_Aspect_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Aspect_Specification_Text
    (Self : aliased in out Implicit_Aspect_Specification)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Aspect_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Aspect_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Aspect_Specification)
      return Boolean;

end Program.Nodes.Aspect_Specifications;
