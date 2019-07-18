--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Exception_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Exception_Declarations is

   pragma Pure (Program.Nodes.Exception_Declarations);

   type Exception_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Exception_Declarations.Exception_Declaration
         and Program.Elements.Exception_Declarations.Exception_Declaration_Text
     with private;

   function Create
    (Names           : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Exception_Declaration;

   type Implicit_Exception_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Exception_Declarations.Exception_Declaration
     with private;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Exception_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Exception_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Exception_Declarations.Exception_Declaration
     with record
        Names   : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;
        Aspects : not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Exception_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Exception_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Names
    (Self : Base_Exception_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;

   overriding function Aspects
    (Self : Base_Exception_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Exception_Declaration
    (Self : Base_Exception_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Exception_Declaration)
      return Boolean;

   type Exception_Declaration is
     new Base_Exception_Declaration
       and Program.Elements.Exception_Declarations.Exception_Declaration_Text
     with record
        Colon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Exception_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Exception_Declaration_Text
    (Self : aliased in out Exception_Declaration)
      return Program.Elements.Exception_Declarations
          .Exception_Declaration_Text_Access;

   overriding function Colon_Token
    (Self : Exception_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Exception_Token
    (Self : Exception_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Exception_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Exception_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Exception_Declaration is
     new Base_Exception_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Exception_Declaration_Text
    (Self : aliased in out Implicit_Exception_Declaration)
      return Program.Elements.Exception_Declarations
          .Exception_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Exception_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Exception_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Exception_Declaration)
      return Boolean;

end Program.Nodes.Exception_Declarations;
