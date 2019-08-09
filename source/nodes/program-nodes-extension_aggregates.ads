--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Record_Component_Associations;
with Program.Elements.Extension_Aggregates;
with Program.Element_Visitors;

package Program.Nodes.Extension_Aggregates is

   pragma Preelaborate;

   type Extension_Aggregate is
     new Program.Nodes.Node
         and Program.Elements.Extension_Aggregates.Extension_Aggregate
         and Program.Elements.Extension_Aggregates.Extension_Aggregate_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ancestor            : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Extension_Aggregate;

   type Implicit_Extension_Aggregate is
     new Program.Nodes.Node
         and Program.Elements.Extension_Aggregates.Extension_Aggregate
     with private;

   function Create
    (Ancestor             : not null Program.Elements.Expressions
         .Expression_Access;
     Components           : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Extension_Aggregate
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Extension_Aggregate is
     abstract new Program.Nodes.Node
       and Program.Elements.Extension_Aggregates.Extension_Aggregate
     with record
        Ancestor   : not null Program.Elements.Expressions.Expression_Access;
        Components : Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Extension_Aggregate'Class);

   overriding procedure Visit
    (Self    : not null access Base_Extension_Aggregate;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Ancestor
    (Self : Base_Extension_Aggregate)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Components
    (Self : Base_Extension_Aggregate)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access;

   overriding function Is_Extension_Aggregate_Element
    (Self : Base_Extension_Aggregate)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Extension_Aggregate)
      return Boolean;

   type Extension_Aggregate is
     new Base_Extension_Aggregate
       and Program.Elements.Extension_Aggregates.Extension_Aggregate_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token          : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Extension_Aggregate_Text
    (Self : aliased in out Extension_Aggregate)
      return Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Extension_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Extension_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Extension_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Extension_Aggregate is
     new Base_Extension_Aggregate
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Extension_Aggregate_Text
    (Self : aliased in out Implicit_Extension_Aggregate)
      return Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Extension_Aggregate)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Extension_Aggregate)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Extension_Aggregate)
      return Boolean;

end Program.Nodes.Extension_Aggregates;
