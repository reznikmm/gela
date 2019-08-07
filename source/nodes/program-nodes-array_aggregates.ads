--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Array_Component_Associations;
with Program.Elements.Array_Aggregates;
with Program.Element_Visitors;

package Program.Nodes.Array_Aggregates is

   pragma Preelaborate;

   type Array_Aggregate is
     new Program.Nodes.Node
         and Program.Elements.Array_Aggregates.Array_Aggregate
         and Program.Elements.Array_Aggregates.Array_Aggregate_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : Program.Elements.Array_Component_Associations
         .Array_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Array_Aggregate;

   type Implicit_Array_Aggregate is
     new Program.Nodes.Node
         and Program.Elements.Array_Aggregates.Array_Aggregate
     with private;

   function Create
    (Components           : Program.Elements.Array_Component_Associations
         .Array_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Array_Aggregate
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Array_Aggregate is
     abstract new Program.Nodes.Node
       and Program.Elements.Array_Aggregates.Array_Aggregate
     with record
        Components : Program.Elements.Array_Component_Associations
          .Array_Component_Association_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Array_Aggregate'Class);

   overriding procedure Visit
    (Self    : not null access Base_Array_Aggregate;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Components
    (Self : Base_Array_Aggregate)
      return Program.Elements.Array_Component_Associations
          .Array_Component_Association_Vector_Access;

   overriding function Is_Array_Aggregate
    (Self : Base_Array_Aggregate)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Array_Aggregate)
      return Boolean;

   type Array_Aggregate is
     new Base_Array_Aggregate
       and Program.Elements.Array_Aggregates.Array_Aggregate_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Array_Aggregate_Text
    (Self : aliased in out Array_Aggregate)
      return Program.Elements.Array_Aggregates.Array_Aggregate_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Array_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Array_Aggregate)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Array_Aggregate is
     new Base_Array_Aggregate
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Array_Aggregate_Text
    (Self : aliased in out Implicit_Array_Aggregate)
      return Program.Elements.Array_Aggregates.Array_Aggregate_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Array_Aggregate)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Array_Aggregate)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Array_Aggregate)
      return Boolean;

end Program.Nodes.Array_Aggregates;
