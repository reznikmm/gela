--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Procedure_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Procedure_Declarations is

   pragma Preelaborate;

   type Procedure_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Procedure_Declarations.Procedure_Declaration
         and Program.Elements.Procedure_Declarations.Procedure_Declaration_Text
     with private;

   function Create
    (Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Procedure_Declaration;

   type Implicit_Procedure_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Procedure_Declarations.Procedure_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Has_Abstract         : Boolean := False)
      return Implicit_Procedure_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Procedure_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Procedure_Declarations.Procedure_Declaration
     with record
        Name       : not null Program.Elements.Defining_Names
          .Defining_Name_Access;
        Parameters : Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Aspects    : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Procedure_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Procedure_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Procedure_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access;

   overriding function Parameters
    (Self : Base_Procedure_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Aspects
    (Self : Base_Procedure_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Procedure_Declaration_Element
    (Self : Base_Procedure_Declaration)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Procedure_Declaration)
      return Boolean;

   type Procedure_Declaration is
     new Base_Procedure_Declaration
       and Program.Elements.Procedure_Declarations.Procedure_Declaration_Text
     with record
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        Procedure_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
        Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Procedure_Declaration_Text
    (Self : aliased in out Procedure_Declaration)
      return Program.Elements.Procedure_Declarations
          .Procedure_Declaration_Text_Access;

   overriding function Not_Token
    (Self : Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Overriding_Token
    (Self : Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Procedure_Token
    (Self : Procedure_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Abstract_Token
    (Self : Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Procedure_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not (Self : Procedure_Declaration) return Boolean;

   overriding function Has_Overriding
    (Self : Procedure_Declaration)
      return Boolean;

   overriding function Has_Abstract
    (Self : Procedure_Declaration)
      return Boolean;

   type Implicit_Procedure_Declaration is
     new Base_Procedure_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not              : Boolean;
        Has_Overriding       : Boolean;
        Has_Abstract         : Boolean;
     end record;

   overriding function To_Procedure_Declaration_Text
    (Self : aliased in out Implicit_Procedure_Declaration)
      return Program.Elements.Procedure_Declarations
          .Procedure_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Procedure_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Procedure_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Procedure_Declaration)
      return Boolean;

   overriding function Has_Not
    (Self : Implicit_Procedure_Declaration)
      return Boolean;

   overriding function Has_Overriding
    (Self : Implicit_Procedure_Declaration)
      return Boolean;

   overriding function Has_Abstract
    (Self : Implicit_Procedure_Declaration)
      return Boolean;

end Program.Nodes.Procedure_Declarations;
