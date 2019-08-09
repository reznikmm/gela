--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Enumeration_Literal_Specifications;
with Program.Elements.Enumeration_Types;
with Program.Element_Visitors;

package Program.Nodes.Enumeration_Types is

   pragma Preelaborate;

   type Enumeration_Type is
     new Program.Nodes.Node
         and Program.Elements.Enumeration_Types.Enumeration_Type
         and Program.Elements.Enumeration_Types.Enumeration_Type_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Literals            : not null Program.Elements
         .Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Enumeration_Type;

   type Implicit_Enumeration_Type is
     new Program.Nodes.Node
         and Program.Elements.Enumeration_Types.Enumeration_Type
     with private;

   function Create
    (Literals             : not null Program.Elements
         .Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Enumeration_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Enumeration_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Enumeration_Types.Enumeration_Type
     with record
        Literals : not null Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Enumeration_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Enumeration_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Literals
    (Self : Base_Enumeration_Type)
      return not null Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Vector_Access;

   overriding function Is_Enumeration_Type_Element
    (Self : Base_Enumeration_Type)
      return Boolean;

   overriding function Is_Type_Definition_Element
    (Self : Base_Enumeration_Type)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Enumeration_Type)
      return Boolean;

   type Enumeration_Type is
     new Base_Enumeration_Type
       and Program.Elements.Enumeration_Types.Enumeration_Type_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Enumeration_Type_Text
    (Self : aliased in out Enumeration_Type)
      return Program.Elements.Enumeration_Types.Enumeration_Type_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Enumeration_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Enumeration_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Enumeration_Type is
     new Base_Enumeration_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Enumeration_Type_Text
    (Self : aliased in out Implicit_Enumeration_Type)
      return Program.Elements.Enumeration_Types.Enumeration_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Enumeration_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Enumeration_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Enumeration_Type)
      return Boolean;

end Program.Nodes.Enumeration_Types;
