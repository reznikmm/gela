--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Array_Component_Associations;
with Program.Element_Visitors;

package Program.Nodes.Array_Component_Associations is

   pragma Preelaborate;

   type Array_Component_Association is
     new Program.Nodes.Node
         and Program.Elements.Array_Component_Associations
           .Array_Component_Association
         and Program.Elements.Array_Component_Associations
           .Array_Component_Association_Text
     with private;

   function Create
    (Choices     : Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return Array_Component_Association;

   type Implicit_Array_Component_Association is
     new Program.Nodes.Node
         and Program.Elements.Array_Component_Associations
           .Array_Component_Association
     with private;

   function Create
    (Choices              : Program.Element_Vectors.Element_Vector_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Array_Component_Association
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Array_Component_Association is
     abstract new Program.Nodes.Node
       and Program.Elements.Array_Component_Associations
         .Array_Component_Association
     with record
        Choices    : Program.Element_Vectors.Element_Vector_Access;
        Expression : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Array_Component_Association'Class);

   overriding procedure Visit
    (Self    : not null access Base_Array_Component_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Choices
    (Self : Base_Array_Component_Association)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Expression
    (Self : Base_Array_Component_Association)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Array_Component_Association
    (Self : Base_Array_Component_Association)
      return Boolean;

   overriding function Is_Association
    (Self : Base_Array_Component_Association)
      return Boolean;

   type Array_Component_Association is
     new Base_Array_Component_Association
       and Program.Elements.Array_Component_Associations
         .Array_Component_Association_Text
     with record
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Box_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Array_Component_Association_Text
    (Self : aliased in out Array_Component_Association)
      return Program.Elements.Array_Component_Associations
          .Array_Component_Association_Text_Access;

   overriding function Arrow_Token
    (Self : Array_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Box_Token
    (Self : Array_Component_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Array_Component_Association is
     new Base_Array_Component_Association
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Array_Component_Association_Text
    (Self : aliased in out Implicit_Array_Component_Association)
      return Program.Elements.Array_Component_Associations
          .Array_Component_Association_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Array_Component_Association)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Array_Component_Association)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Array_Component_Association)
      return Boolean;

end Program.Nodes.Array_Component_Associations;
