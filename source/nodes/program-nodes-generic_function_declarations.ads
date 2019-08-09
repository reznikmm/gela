--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Defining_Names;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Generic_Function_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Generic_Function_Declarations is

   pragma Preelaborate;

   type Generic_Function_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Generic_Function_Declarations
           .Generic_Function_Declaration
         and Program.Elements.Generic_Function_Declarations
           .Generic_Function_Declaration_Text
     with private;

   function Create
    (Generic_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters   : Program.Element_Vectors.Element_Vector_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Generic_Function_Declaration;

   type Implicit_Generic_Function_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Generic_Function_Declarations
           .Generic_Function_Declaration
     with private;

   function Create
    (Formal_Parameters    : Program.Element_Vectors.Element_Vector_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Generic_Function_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Generic_Function_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Generic_Function_Declarations
         .Generic_Function_Declaration
     with record
        Formal_Parameters : Program.Element_Vectors.Element_Vector_Access;
        Name              : not null Program.Elements.Defining_Names
          .Defining_Name_Access;
        Parameters        : Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Result_Subtype    : not null Program.Elements.Element_Access;
        Aspects           : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Generic_Function_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Generic_Function_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Formal_Parameters
    (Self : Base_Generic_Function_Declaration)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Name
    (Self : Base_Generic_Function_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access;

   overriding function Parameters
    (Self : Base_Generic_Function_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Result_Subtype
    (Self : Base_Generic_Function_Declaration)
      return not null Program.Elements.Element_Access;

   overriding function Aspects
    (Self : Base_Generic_Function_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Generic_Function_Declaration_Element
    (Self : Base_Generic_Function_Declaration)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Generic_Function_Declaration)
      return Boolean;

   type Generic_Function_Declaration is
     new Base_Generic_Function_Declaration
       and Program.Elements.Generic_Function_Declarations
         .Generic_Function_Declaration_Text
     with record
        Generic_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Function_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Return_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Generic_Function_Declaration_Text
    (Self : aliased in out Generic_Function_Declaration)
      return Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration_Text_Access;

   overriding function Generic_Token
    (Self : Generic_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Function_Token
    (Self : Generic_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Generic_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Generic_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Return_Token
    (Self : Generic_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Not_Token
    (Self : Generic_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Generic_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Generic_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Generic_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null
    (Self : Generic_Function_Declaration)
      return Boolean;

   type Implicit_Generic_Function_Declaration is
     new Base_Generic_Function_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
     end record;

   overriding function To_Generic_Function_Declaration_Text
    (Self : aliased in out Implicit_Generic_Function_Declaration)
      return Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Generic_Function_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Generic_Function_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Generic_Function_Declaration)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Generic_Function_Declaration)
      return Boolean;

end Program.Nodes.Generic_Function_Declarations;
