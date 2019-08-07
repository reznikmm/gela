--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Choice_Parameter_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Element_Visitors;

package Program.Nodes.Exception_Handlers is

   pragma Preelaborate;

   type Exception_Handler is
     new Program.Nodes.Node
         and Program.Elements.Exception_Handlers.Exception_Handler
         and Program.Elements.Exception_Handlers.Exception_Handler_Text
     with private;

   function Create
    (When_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Choice_Parameter : Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access;
     Choices          : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements       : not null Program.Element_Vectors.Element_Vector_Access)
      return Exception_Handler;

   type Implicit_Exception_Handler is
     new Program.Nodes.Node
         and Program.Elements.Exception_Handlers.Exception_Handler
     with private;

   function Create
    (Choice_Parameter     : Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Exception_Handler
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Exception_Handler is
     abstract new Program.Nodes.Node
       and Program.Elements.Exception_Handlers.Exception_Handler
     with record
        Choice_Parameter : Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access;
        Choices          : not null Program.Element_Vectors
          .Element_Vector_Access;
        Statements       : not null Program.Element_Vectors
          .Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Exception_Handler'Class);

   overriding procedure Visit
    (Self    : not null access Base_Exception_Handler;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Choice_Parameter
    (Self : Base_Exception_Handler)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access;

   overriding function Choices
    (Self : Base_Exception_Handler)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Statements
    (Self : Base_Exception_Handler)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Exception_Handler
    (Self : Base_Exception_Handler)
      return Boolean;

   type Exception_Handler is
     new Base_Exception_Handler
       and Program.Elements.Exception_Handlers.Exception_Handler_Text
     with record
        When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
        Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Exception_Handler_Text
    (Self : aliased in out Exception_Handler)
      return Program.Elements.Exception_Handlers.Exception_Handler_Text_Access;

   overriding function When_Token
    (Self : Exception_Handler)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Arrow_Token
    (Self : Exception_Handler)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Exception_Handler is
     new Base_Exception_Handler
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Exception_Handler_Text
    (Self : aliased in out Implicit_Exception_Handler)
      return Program.Elements.Exception_Handlers.Exception_Handler_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Exception_Handler)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Exception_Handler)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Exception_Handler)
      return Boolean;

end Program.Nodes.Exception_Handlers;
