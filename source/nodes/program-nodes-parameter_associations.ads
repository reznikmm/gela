--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Associations;
with Program.Element_Visitors;

package Program.Nodes.Parameter_Associations is

   pragma Preelaborate;

   type Parameter_Association is
     new Program.Nodes.Node
         and Program.Elements.Parameter_Associations.Parameter_Association
         and Program.Elements.Parameter_Associations.Parameter_Association_Text
     with private;

   function Create
    (Formal_Parameter : Program.Elements.Expressions.Expression_Access;
     Arrow_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Actual_Parameter : not null Program.Elements.Expressions
         .Expression_Access)
      return Parameter_Association;

   type Implicit_Parameter_Association is
     new Program.Nodes.Node
         and Program.Elements.Parameter_Associations.Parameter_Association
     with private;

   function Create
    (Formal_Parameter     : Program.Elements.Expressions.Expression_Access;
     Actual_Parameter     : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Parameter_Association
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Parameter_Association is
     abstract new Program.Nodes.Node
       and Program.Elements.Parameter_Associations.Parameter_Association
     with record
        Formal_Parameter : Program.Elements.Expressions.Expression_Access;
        Actual_Parameter : not null Program.Elements.Expressions
          .Expression_Access;
     end record;

   procedure Initialize (Self : in out Base_Parameter_Association'Class);

   overriding procedure Visit
    (Self    : not null access Base_Parameter_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Formal_Parameter
    (Self : Base_Parameter_Association)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Actual_Parameter
    (Self : Base_Parameter_Association)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Parameter_Association
    (Self : Base_Parameter_Association)
      return Boolean;

   overriding function Is_Association
    (Self : Base_Parameter_Association)
      return Boolean;

   type Parameter_Association is
     new Base_Parameter_Association
       and Program.Elements.Parameter_Associations.Parameter_Association_Text
     with record
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Parameter_Association_Text
    (Self : in out Parameter_Association)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Text_Access;

   overriding function Arrow_Token
    (Self : Parameter_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Parameter_Association is
     new Base_Parameter_Association
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Parameter_Association_Text
    (Self : in out Implicit_Parameter_Association)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Parameter_Association)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Parameter_Association)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Parameter_Association)
      return Boolean;

end Program.Nodes.Parameter_Associations;
