--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Formal_Function_Access_Types;
with Program.Element_Visitors;

package Program.Nodes.Formal_Function_Access_Types is

   pragma Preelaborate;

   type Formal_Function_Access_Type is
     new Program.Nodes.Node
         and Program.Elements.Formal_Function_Access_Types
           .Formal_Function_Access_Type
         and Program.Elements.Formal_Function_Access_Types
           .Formal_Function_Access_Type_Text
     with private;

   function Create
    (Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access)
      return Formal_Function_Access_Type;

   type Implicit_Formal_Function_Access_Type is
     new Program.Nodes.Node
         and Program.Elements.Formal_Function_Access_Types
           .Formal_Function_Access_Type
     with private;

   function Create
    (Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_Protected        : Boolean := False;
     Has_Not_Null_2       : Boolean := False)
      return Implicit_Formal_Function_Access_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Function_Access_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Function_Access_Types
         .Formal_Function_Access_Type
     with record
        Parameters     : Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Result_Subtype : not null Program.Elements.Element_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Function_Access_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Function_Access_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Parameters
    (Self : Base_Formal_Function_Access_Type)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Result_Subtype
    (Self : Base_Formal_Function_Access_Type)
      return not null Program.Elements.Element_Access;

   overriding function Is_Formal_Function_Access_Type_Element
    (Self : Base_Formal_Function_Access_Type)
      return Boolean;

   overriding function Is_Formal_Access_Type_Element
    (Self : Base_Formal_Function_Access_Type)
      return Boolean;

   overriding function Is_Formal_Type_Definition_Element
    (Self : Base_Formal_Function_Access_Type)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Formal_Function_Access_Type)
      return Boolean;

   type Formal_Function_Access_Type is
     new Base_Formal_Function_Access_Type
       and Program.Elements.Formal_Function_Access_Types
         .Formal_Function_Access_Type_Text
     with record
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Access_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Function_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Return_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Formal_Function_Access_Type_Text
    (Self : aliased in out Formal_Function_Access_Type)
      return Program.Elements.Formal_Function_Access_Types
          .Formal_Function_Access_Type_Text_Access;

   overriding function Not_Token
    (Self : Formal_Function_Access_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Formal_Function_Access_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Access_Token
    (Self : Formal_Function_Access_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Protected_Token
    (Self : Formal_Function_Access_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Function_Token
    (Self : Formal_Function_Access_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Formal_Function_Access_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Formal_Function_Access_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Return_Token
    (Self : Formal_Function_Access_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Not_Token_2
    (Self : Formal_Function_Access_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token_2
    (Self : Formal_Function_Access_Type)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null
    (Self : Formal_Function_Access_Type)
      return Boolean;

   overriding function Has_Protected
    (Self : Formal_Function_Access_Type)
      return Boolean;

   overriding function Has_Not_Null_2
    (Self : Formal_Function_Access_Type)
      return Boolean;

   type Implicit_Formal_Function_Access_Type is
     new Base_Formal_Function_Access_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
        Has_Protected        : Boolean;
        Has_Not_Null_2       : Boolean;
     end record;

   overriding function To_Formal_Function_Access_Type_Text
    (Self : aliased in out Implicit_Formal_Function_Access_Type)
      return Program.Elements.Formal_Function_Access_Types
          .Formal_Function_Access_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Function_Access_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Function_Access_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Function_Access_Type)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Formal_Function_Access_Type)
      return Boolean;

   overriding function Has_Protected
    (Self : Implicit_Formal_Function_Access_Type)
      return Boolean;

   overriding function Has_Not_Null_2
    (Self : Implicit_Formal_Function_Access_Type)
      return Boolean;

end Program.Nodes.Formal_Function_Access_Types;
