--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Record_Component_Associations;
with Program.Elements.Record_Aggregates;
with Program.Element_Visitors;

package Program.Nodes.Record_Aggregates is

   pragma Preelaborate;

   type Record_Aggregate is
     new Program.Nodes.Node
         and Program.Elements.Record_Aggregates.Record_Aggregate
         and Program.Elements.Record_Aggregates.Record_Aggregate_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Record_Aggregate;

   type Implicit_Record_Aggregate is
     new Program.Nodes.Node
         and Program.Elements.Record_Aggregates.Record_Aggregate
     with private;

   function Create
    (Components           : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Aggregate
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Record_Aggregate is
     abstract new Program.Nodes.Node
       and Program.Elements.Record_Aggregates.Record_Aggregate
     with record
        Components : Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Record_Aggregate'Class);

   overriding procedure Visit
    (Self    : not null access Base_Record_Aggregate;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Components
    (Self : Base_Record_Aggregate)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access;

   overriding function Is_Record_Aggregate
    (Self : Base_Record_Aggregate)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Record_Aggregate)
      return Boolean;

   type Record_Aggregate is
     new Base_Record_Aggregate
       and Program.Elements.Record_Aggregates.Record_Aggregate_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Record_Aggregate_Text
    (Self : aliased in out Record_Aggregate)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Record_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Record_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Record_Aggregate is
     new Base_Record_Aggregate
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Record_Aggregate_Text
    (Self : aliased in out Implicit_Record_Aggregate)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Aggregate)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Aggregate)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Aggregate)
      return Boolean;

end Program.Nodes.Record_Aggregates;
