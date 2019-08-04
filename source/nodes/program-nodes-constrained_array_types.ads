--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Component_Definitions;
with Program.Elements.Constrained_Array_Types;
with Program.Element_Visitors;

package Program.Nodes.Constrained_Array_Types is

   pragma Pure (Program.Nodes.Constrained_Array_Types);

   type Constrained_Array_Type is
     new Program.Nodes.Node
         and Program.Elements.Constrained_Array_Types.Constrained_Array_Type
         and Program.Elements.Constrained_Array_Types
           .Constrained_Array_Type_Text
     with private;

   function Create
    (Array_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Index_Subtypes       : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Of_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access)
      return Constrained_Array_Type;

   type Implicit_Constrained_Array_Type is
     new Program.Nodes.Node
         and Program.Elements.Constrained_Array_Types.Constrained_Array_Type
     with private;

   function Create
    (Index_Subtypes       : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Constrained_Array_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Constrained_Array_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Constrained_Array_Types.Constrained_Array_Type
     with record
        Index_Subtypes       : not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Vector_Access;
        Component_Definition : not null Program.Elements.Component_Definitions
          .Component_Definition_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Constrained_Array_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Constrained_Array_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Index_Subtypes
    (Self : Base_Constrained_Array_Type)
      return not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Vector_Access;

   overriding function Component_Definition
    (Self : Base_Constrained_Array_Type)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access;

   overriding function Is_Constrained_Array_Type
    (Self : Base_Constrained_Array_Type)
      return Boolean;

   overriding function Is_Type_Definition
    (Self : Base_Constrained_Array_Type)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Constrained_Array_Type)
      return Boolean;

   type Constrained_Array_Type is
     new Base_Constrained_Array_Type
       and Program.Elements.Constrained_Array_Types.Constrained_Array_Type_Text
     with record
        Array_Token         : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Of_Token            : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Constrained_Array_Type_Text
    (Self : aliased in out Constrained_Array_Type)
      return Program.Elements.Constrained_Array_Types
          .Constrained_Array_Type_Text_Access;

   overriding function Array_Token
    (Self : Constrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Constrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Constrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Of_Token
    (Self : Constrained_Array_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Constrained_Array_Type is
     new Base_Constrained_Array_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Constrained_Array_Type_Text
    (Self : aliased in out Implicit_Constrained_Array_Type)
      return Program.Elements.Constrained_Array_Types
          .Constrained_Array_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Constrained_Array_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Constrained_Array_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Constrained_Array_Type)
      return Boolean;

end Program.Nodes.Constrained_Array_Types;
