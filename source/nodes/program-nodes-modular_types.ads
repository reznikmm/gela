--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Modular_Types;
with Program.Element_Visitors;

package Program.Nodes.Modular_Types is

   pragma Preelaborate;

   type Modular_Type is
     new Program.Nodes.Node and Program.Elements.Modular_Types.Modular_Type
         and Program.Elements.Modular_Types.Modular_Type_Text
     with private;

   function Create
    (Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Modulus   : not null Program.Elements.Expressions.Expression_Access)
      return Modular_Type;

   type Implicit_Modular_Type is
     new Program.Nodes.Node and Program.Elements.Modular_Types.Modular_Type
     with private;

   function Create
    (Modulus              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Modular_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Modular_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Modular_Types.Modular_Type
     with record
        Modulus : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Modular_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Modular_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Modulus
    (Self : Base_Modular_Type)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Modular_Type_Element
    (Self : Base_Modular_Type)
      return Boolean;

   overriding function Is_Type_Definition_Element
    (Self : Base_Modular_Type)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Modular_Type)
      return Boolean;

   type Modular_Type is
     new Base_Modular_Type and Program.Elements.Modular_Types.Modular_Type_Text
     with record
        Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Modular_Type_Text
    (Self : aliased in out Modular_Type)
      return Program.Elements.Modular_Types.Modular_Type_Text_Access;

   overriding function Mod_Token
    (Self : Modular_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Modular_Type is
     new Base_Modular_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Modular_Type_Text
    (Self : aliased in out Implicit_Modular_Type)
      return Program.Elements.Modular_Types.Modular_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Modular_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Modular_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Modular_Type)
      return Boolean;

end Program.Nodes.Modular_Types;
