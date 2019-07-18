--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Formal_Package_Associations;
with Program.Element_Visitors;

package Program.Nodes.Formal_Package_Associations is

   pragma Pure (Program.Nodes.Formal_Package_Associations);

   type Formal_Package_Association is
     new Program.Nodes.Node
         and Program.Elements.Formal_Package_Associations
           .Formal_Package_Association
         and Program.Elements.Formal_Package_Associations
           .Formal_Package_Association_Text
     with private;

   function Create
    (Formal_Parameter : Program.Elements.Expressions.Expression_Access;
     Arrow_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Actual_Parameter : Program.Elements.Expressions.Expression_Access;
     Box_Token        : Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Package_Association;

   type Implicit_Formal_Package_Association is
     new Program.Nodes.Node
         and Program.Elements.Formal_Package_Associations
           .Formal_Package_Association
     with private;

   function Create
    (Formal_Parameter     : Program.Elements.Expressions.Expression_Access;
     Actual_Parameter     : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Package_Association
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Package_Association is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Package_Associations
         .Formal_Package_Association
     with record
        Formal_Parameter : Program.Elements.Expressions.Expression_Access;
        Actual_Parameter : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Package_Association'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Package_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Formal_Parameter
    (Self : Base_Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Actual_Parameter
    (Self : Base_Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Formal_Package_Association
    (Self : Base_Formal_Package_Association)
      return Boolean;

   overriding function Is_Association
    (Self : Base_Formal_Package_Association)
      return Boolean;

   type Formal_Package_Association is
     new Base_Formal_Package_Association
       and Program.Elements.Formal_Package_Associations
         .Formal_Package_Association_Text
     with record
        Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Box_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Formal_Package_Association_Text
    (Self : aliased in out Formal_Package_Association)
      return Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Text_Access;

   overriding function Arrow_Token
    (Self : Formal_Package_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Box_Token
    (Self : Formal_Package_Association)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Formal_Package_Association is
     new Base_Formal_Package_Association
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Formal_Package_Association_Text
    (Self : aliased in out Implicit_Formal_Package_Association)
      return Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Package_Association)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Package_Association)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Package_Association)
      return Boolean;

end Program.Nodes.Formal_Package_Associations;
