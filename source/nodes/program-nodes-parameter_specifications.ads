--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Parameter_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Parameter_Specifications is

   pragma Preelaborate;

   type Parameter_Specification is
     new Program.Nodes.Node
         and Program.Elements.Parameter_Specifications.Parameter_Specification
         and Program.Elements.Parameter_Specifications
           .Parameter_Specification_Text
     with private;

   function Create
    (Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     In_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Out_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Parameter_Subtype  : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access)
      return Parameter_Specification;

   type Implicit_Parameter_Specification is
     new Program.Nodes.Node
         and Program.Elements.Parameter_Specifications.Parameter_Specification
     with private;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Parameter_Subtype    : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Aliased          : Boolean := False;
     Has_In               : Boolean := False;
     Has_Out              : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Parameter_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Parameter_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Parameter_Specifications.Parameter_Specification
     with record
        Names              : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;
        Parameter_Subtype  : not null Program.Elements.Element_Access;
        Default_Expression : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Parameter_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Parameter_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Names
    (Self : Base_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;

   overriding function Parameter_Subtype
    (Self : Base_Parameter_Specification)
      return not null Program.Elements.Element_Access;

   overriding function Default_Expression
    (Self : Base_Parameter_Specification)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Parameter_Specification
    (Self : Base_Parameter_Specification)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Parameter_Specification)
      return Boolean;

   type Parameter_Specification is
     new Base_Parameter_Specification
       and Program.Elements.Parameter_Specifications
         .Parameter_Specification_Text
     with record
        Colon_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Aliased_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        In_Token         : Program.Lexical_Elements.Lexical_Element_Access;
        Out_Token        : Program.Lexical_Elements.Lexical_Element_Access;
        Not_Token        : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Parameter_Specification_Text
    (Self : aliased in out Parameter_Specification)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Text_Access;

   overriding function Colon_Token
    (Self : Parameter_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Aliased_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function In_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Out_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Not_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Assignment_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Aliased
    (Self : Parameter_Specification)
      return Boolean;

   overriding function Has_In (Self : Parameter_Specification) return Boolean;

   overriding function Has_Out (Self : Parameter_Specification) return Boolean;

   overriding function Has_Not_Null
    (Self : Parameter_Specification)
      return Boolean;

   type Implicit_Parameter_Specification is
     new Base_Parameter_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Aliased          : Boolean;
        Has_In               : Boolean;
        Has_Out              : Boolean;
        Has_Not_Null         : Boolean;
     end record;

   overriding function To_Parameter_Specification_Text
    (Self : aliased in out Implicit_Parameter_Specification)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Parameter_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Parameter_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Parameter_Specification)
      return Boolean;

   overriding function Has_Aliased
    (Self : Implicit_Parameter_Specification)
      return Boolean;

   overriding function Has_In
    (Self : Implicit_Parameter_Specification)
      return Boolean;

   overriding function Has_Out
    (Self : Implicit_Parameter_Specification)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Parameter_Specification)
      return Boolean;

end Program.Nodes.Parameter_Specifications;
