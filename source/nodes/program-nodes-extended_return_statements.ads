--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Return_Object_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Extended_Return_Statements;
with Program.Element_Visitors;

package Program.Nodes.Extended_Return_Statements is

   pragma Preelaborate;

   type Extended_Return_Statement is
     new Program.Nodes.Node
         and Program.Elements.Extended_Return_Statements
           .Extended_Return_Statement
         and Program.Elements.Extended_Return_Statements
           .Extended_Return_Statement_Text
     with private;

   function Create
    (Return_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Return_Object      : not null Program.Elements
         .Return_Object_Specifications.Return_Object_Specification_Access;
     Do_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Statements         : Program.Element_Vectors.Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token_2     : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Extended_Return_Statement;

   type Implicit_Extended_Return_Statement is
     new Program.Nodes.Node
         and Program.Elements.Extended_Return_Statements
           .Extended_Return_Statement
     with private;

   function Create
    (Return_Object        : not null Program.Elements
         .Return_Object_Specifications.Return_Object_Specification_Access;
     Statements           : Program.Element_Vectors.Element_Vector_Access;
     Exception_Handlers   : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Extended_Return_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Extended_Return_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Extended_Return_Statements
         .Extended_Return_Statement
     with record
        Return_Object      : not null Program.Elements
          .Return_Object_Specifications.Return_Object_Specification_Access;
        Statements         : Program.Element_Vectors.Element_Vector_Access;
        Exception_Handlers : Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Extended_Return_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Extended_Return_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Return_Object
    (Self : Base_Extended_Return_Statement)
      return not null Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access;

   overriding function Statements
    (Self : Base_Extended_Return_Statement)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Exception_Handlers
    (Self : Base_Extended_Return_Statement)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;

   overriding function Is_Extended_Return_Statement_Element
    (Self : Base_Extended_Return_Statement)
      return Boolean;

   overriding function Is_Statement_Element
    (Self : Base_Extended_Return_Statement)
      return Boolean;

   type Extended_Return_Statement is
     new Base_Extended_Return_Statement
       and Program.Elements.Extended_Return_Statements
         .Extended_Return_Statement_Text
     with record
        Return_Token    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Do_Token        : Program.Lexical_Elements.Lexical_Element_Access;
        Exception_Token : Program.Lexical_Elements.Lexical_Element_Access;
        End_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        Return_Token_2  : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Extended_Return_Statement_Text
    (Self : aliased in out Extended_Return_Statement)
      return Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Text_Access;

   overriding function Return_Token
    (Self : Extended_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Do_Token
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Exception_Token
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Return_Token_2
    (Self : Extended_Return_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Extended_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Extended_Return_Statement is
     new Base_Extended_Return_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Extended_Return_Statement_Text
    (Self : aliased in out Implicit_Extended_Return_Statement)
      return Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Extended_Return_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Extended_Return_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Extended_Return_Statement)
      return Boolean;

end Program.Nodes.Extended_Return_Statements;
