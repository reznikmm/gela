--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Formal_Interface_Types;
with Program.Element_Visitors;

package Program.Nodes.Formal_Interface_Types is

   pragma Pure (Program.Nodes.Formal_Interface_Types);

   type Formal_Interface_Type is
     new Program.Nodes.Node
         and Program.Elements.Formal_Interface_Types.Formal_Interface_Type
         and Program.Elements.Formal_Interface_Types.Formal_Interface_Type_Text
     with private;

   function Create
    (Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Task_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Protected_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Interface_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : not null Program.Elements.Expressions
         .Expression_Vector_Access)
      return Formal_Interface_Type;

   type Implicit_Formal_Interface_Type is
     new Program.Nodes.Node
         and Program.Elements.Formal_Interface_Types.Formal_Interface_Type
     with private;

   function Create
    (Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Task             : Boolean := False;
     Has_Protected        : Boolean := False;
     Has_Synchronized     : Boolean := False)
      return Implicit_Formal_Interface_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Interface_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Interface_Types.Formal_Interface_Type
     with record
        Progenitors : not null Program.Elements.Expressions
          .Expression_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Interface_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Interface_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Progenitors
    (Self : Base_Formal_Interface_Type)
      return not null Program.Elements.Expressions.Expression_Vector_Access;

   overriding function Is_Formal_Interface_Type
    (Self : Base_Formal_Interface_Type)
      return Boolean;

   overriding function Is_Formal_Type_Definition
    (Self : Base_Formal_Interface_Type)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Formal_Interface_Type)
      return Boolean;

   type Formal_Interface_Type is
     new Base_Formal_Interface_Type
       and Program.Elements.Formal_Interface_Types.Formal_Interface_Type_Text
     with record
        Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Task_Token         : Program.Lexical_Elements.Lexical_Element_Access;
        Protected_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Interface_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Formal_Interface_Type_Text
    (Self : aliased in out Formal_Interface_Type)
      return Program.Elements.Formal_Interface_Types
          .Formal_Interface_Type_Text_Access;

   overriding function Limited_Token
    (Self : Formal_Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Task_Token
    (Self : Formal_Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Protected_Token
    (Self : Formal_Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Synchronized_Token
    (Self : Formal_Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Interface_Token
    (Self : Formal_Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function And_Token
    (Self : Formal_Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Limited
    (Self : Formal_Interface_Type)
      return Boolean;

   overriding function Has_Task (Self : Formal_Interface_Type) return Boolean;

   overriding function Has_Protected
    (Self : Formal_Interface_Type)
      return Boolean;

   overriding function Has_Synchronized
    (Self : Formal_Interface_Type)
      return Boolean;

   type Implicit_Formal_Interface_Type is
     new Base_Formal_Interface_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Limited          : Boolean;
        Has_Task             : Boolean;
        Has_Protected        : Boolean;
        Has_Synchronized     : Boolean;
     end record;

   overriding function To_Formal_Interface_Type_Text
    (Self : aliased in out Implicit_Formal_Interface_Type)
      return Program.Elements.Formal_Interface_Types
          .Formal_Interface_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Interface_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Interface_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Interface_Type)
      return Boolean;

   overriding function Has_Limited
    (Self : Implicit_Formal_Interface_Type)
      return Boolean;

   overriding function Has_Task
    (Self : Implicit_Formal_Interface_Type)
      return Boolean;

   overriding function Has_Protected
    (Self : Implicit_Formal_Interface_Type)
      return Boolean;

   overriding function Has_Synchronized
    (Self : Implicit_Formal_Interface_Type)
      return Boolean;

end Program.Nodes.Formal_Interface_Types;
