--  SPDX-FileCopyrightText: 2020-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Element_Visitors;
with Program.Elements.Call_Statements;
with Program.Elements.Expressions;
with Program.Elements.Function_Calls;
with Program.Elements.Parameter_Associations;
with Program.Elements.Record_Aggregates;
with Program.Elements.Record_Component_Associations;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Discriminant_Associations;
with Program.Elements.Discriminant_Constraints;
with Program.Elements.Index_Constraints;
with Program.Lexical_Elements;

package Program.Nodes.Proxy_Calls is

   pragma Preelaborate;

   type Proxy_Call is new Program.Nodes.Node
     and Program.Elements.Call_Statements.Call_Statement
     and Program.Elements.Call_Statements.Call_Statement_Text
     and Program.Elements.Function_Calls.Function_Call
     and Program.Elements.Function_Calls.Function_Call_Text
     and Program.Elements.Discriminant_Constraints.Discriminant_Constraint
     and Program.Elements.Index_Constraints.Index_Constraint
     and Program.Elements.Record_Aggregates.Record_Aggregate
       with private;
   --  Internal common representation of
   --  * Call_Statement
   --  * Function_Call
   --  * Record_Aggregate
   --  * Discriminant_Constraint
   --  * TODO Function_Call, Slice, Type_Conv, etc

   type Proxy_Call_Access is access all Proxy_Call;

   function Create
    (Called_Name         : Program.Elements.Expressions.Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Element_Vectors.Element_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Proxy_Call;

   procedure Turn_To_Function_Call
     (Self        : in out Proxy_Call'Class;
      Called_Name : not null Program.Elements.Expressions.Expression_Access);

   procedure Turn_To_Procedure_Call
     (Self            : in out Proxy_Call'Class;
      Semicolon_Token : Program.Lexical_Elements.Lexical_Element_Access);

   procedure Turn_To_Discriminant_Constraint
     (Self : in out Proxy_Call'Class;
      Mark : out Program.Elements.Expressions.Expression_Access);

   procedure Turn_To_Index_Constraint (Self : in out Proxy_Call'Class);

private

   type Kind is
     (A_Call_Statement,
      A_Function_Call,
      A_Discriminant_Constraint,
      An_Index_Constraint,
      A_Record_Aggregate);

   type Proxy_Call_Text (Parent : not null Proxy_Call_Access) is
     new Program.Elements.Record_Aggregates.Record_Aggregate_Text
     and Program.Elements.Discriminant_Constraints.Discriminant_Constraint_Text
     and Program.Elements.Index_Constraints.Index_Constraint_Text
       with null record;

   type Base_Vector (Parent : not null Proxy_Call_Access) is abstract new
     Program.Element_Vectors.Element_Vector
       with null record;

   overriding function Get_Length (Self : Base_Vector) return Positive;

   overriding function Delimiter
     (Self  : Base_Vector;
      Index : Positive)
     return Program.Lexical_Elements.Lexical_Element_Access;

   type Parameter_Vector is new Base_Vector
     and Program.Elements.Parameter_Associations.Parameter_Association_Vector
   with null record;

   overriding function Element
     (Self  : Parameter_Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access;

   type Discriminant_Association_Vector is new Base_Vector
     and Program.Elements.Discriminant_Associations
           .Discriminant_Association_Vector
   with null record;

   overriding function Element
     (Self  : Discriminant_Association_Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access;

   type Discrete_Range_Vector is new Base_Vector
     and Program.Elements.Discrete_Ranges.Discrete_Range_Vector
   with null record;

   overriding function Element
     (Self  : Discrete_Range_Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access;

   type Proxy_Call is new Program.Nodes.Node
     and Program.Elements.Call_Statements.Call_Statement
     and Program.Elements.Call_Statements.Call_Statement_Text
     and Program.Elements.Function_Calls.Function_Call
     and Program.Elements.Function_Calls.Function_Call_Text
     and Program.Elements.Discriminant_Constraints.Discriminant_Constraint
     and Program.Elements.Index_Constraints.Index_Constraint
     and Program.Elements.Record_Aggregates.Record_Aggregate with
   record
      This        : Proxy_Call_Access := Proxy_Call'Unchecked_Access;
      --  to get r/w access from constant Self parameter
      Current     : Kind;
      Text        : aliased Proxy_Call_Text (Proxy_Call'Unchecked_Access);
      Called_Name : Program.Elements.Expressions.Expression_Access;
      Components  : Program.Element_Vectors.Element_Vector_Access;
      Parameters  : aliased Parameter_Vector (Proxy_Call'Unchecked_Access);
      Discr       : aliased Discriminant_Association_Vector
                              (Proxy_Call'Unchecked_Access);
      Ranges : aliased Discrete_Range_Vector (Proxy_Call'Unchecked_Access);
      Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
      Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
      Semicolon_Token     : Program.Lexical_Elements.Lexical_Element_Access;
   end record;

   overriding procedure Visit
    (Self    : not null access Proxy_Call;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Called_Name (Self : Proxy_Call)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Parameters (Self : Proxy_Call)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;

   overriding function Is_Call_Statement (Self : Proxy_Call) return Boolean;

   overriding function Is_Statement (Self : Proxy_Call) return Boolean;

   overriding function To_Call_Statement_Text
    (Self : aliased in out Proxy_Call)
      return Program.Elements.Call_Statements.Call_Statement_Text_Access;

   overriding function Left_Bracket_Token (Self : Proxy_Call)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token (Self : Proxy_Call)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token (Self : Proxy_Call)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   --  function call

   overriding function Prefix (Self : Proxy_Call)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Function_Call (Self : Proxy_Call)
      return Boolean;

   overriding function To_Function_Call_Text (Self : aliased in out Proxy_Call)
      return Program.Elements.Function_Calls.Function_Call_Text_Access;

   --  Record aggregate

   overriding function Components (Self : Proxy_Call)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access;

   overriding function Is_Record_Aggregate (Self : Proxy_Call) return Boolean;

   overriding function Is_Expression (Self : Proxy_Call) return Boolean;

   overriding function To_Record_Aggregate_Text
     (Self : aliased in out Proxy_Call)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Text_Access;

   overriding function Left_Bracket_Token (Self : Proxy_Call_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token (Self : Proxy_Call_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   --  Discriminant constraint

   overriding function Is_Definition (Self : Proxy_Call) return Boolean;
   overriding function Is_Constraint (Self : Proxy_Call) return Boolean;

   overriding function Is_Discriminant_Constraint (Self : Proxy_Call)
      return Boolean;

   overriding function Discriminants (Self : Proxy_Call)
     return not null Program.Elements.Discriminant_Associations
       .Discriminant_Association_Vector_Access;

   overriding function To_Discriminant_Constraint_Text
     (Self : aliased in out Proxy_Call)
      return Program.Elements.Discriminant_Constraints
        .Discriminant_Constraint_Text_Access;

   --  Index constraint

   overriding function Is_Index_Constraint (Self : Proxy_Call) return Boolean;

   overriding function Ranges (Self : Proxy_Call)
     return not null Program.Elements.Discrete_Ranges
       .Discrete_Range_Vector_Access;

   overriding function To_Index_Constraint_Text
     (Self : aliased in out Proxy_Call)
       return Program.Elements.Index_Constraints.Index_Constraint_Text_Access;

end Program.Nodes.Proxy_Calls;
