--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Record_Component_Associations;
with Program.Element_Visitors;

package Program.Nodes.Record_Component_Associations is

   pragma Preelaborate;

   type Record_Component_Association is
     new Program.Nodes.Node
         and Program.Elements.Record_Component_Associations
           .Record_Component_Association
         and Program.Elements.Record_Component_Associations
           .Record_Component_Association_Text
     with private;

   function Create
    (Choices     : Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return Record_Component_Association;

   type Implicit_Record_Component_Association is
     new Program.Nodes.Node
         and Program.Elements.Record_Component_Associations
           .Record_Component_Association
     with private;

   function Create
    (Choices              : Program.Element_Vectors.Element_Vector_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Component_Association
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Record_Component_Association is
     abstract new Program.Nodes.Node
       and Program.Elements.Record_Component_Associations
         .Record_Component_Association
     with record
        Choices    : Program.Element_Vectors.Element_Vector_Access;
        Expression : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Record_Component_Association'Class);

   overriding procedure Visit
    (Self    : not null access Base_Record_Component_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Choices
    (Self : Base_Record_Component_Association)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Expression
    (Self : Base_Record_Component_Association)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Record_Component_Association_Element
    (Self : Base_Record_Component_Association)
      return Boolean;

   overriding function Is_Association_Element
    (Self : Base_Record_Component_Association)
      return Boolean;

   type Record_Component_Association is
     new Base_Record_Component_Association
       and Program.Elements.Record_Component_Associations
         .Record_Component_Association_Text
     with record
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Box_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Record_Component_Association_Text
    (Self : aliased in out Record_Component_Association)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Text_Access;

   overriding function Arrow_Token
    (Self : Record_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Box_Token
    (Self : Record_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Record_Component_Association is
     new Base_Record_Component_Association
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Record_Component_Association_Text
    (Self : aliased in out Implicit_Record_Component_Association)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Component_Association)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Component_Association)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Component_Association)
      return Boolean;

end Program.Nodes.Record_Component_Associations;
