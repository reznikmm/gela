--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Task_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Task_Definitions is

   pragma Preelaborate;

   type Task_Definition is
     new Program.Nodes.Node
         and Program.Elements.Task_Definitions.Task_Definition
         and Program.Elements.Task_Definitions.Task_Definition_Text
     with private;

   function Create
    (Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access)
      return Task_Definition;

   type Implicit_Task_Definition is
     new Program.Nodes.Node
         and Program.Elements.Task_Definitions.Task_Definition
     with private;

   function Create
    (Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Task_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Task_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Task_Definitions.Task_Definition
     with record
        Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
        Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
        End_Name             : Program.Elements.Identifiers.Identifier_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Task_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Task_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Visible_Declarations
    (Self : Base_Task_Definition)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Private_Declarations
    (Self : Base_Task_Definition)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function End_Name
    (Self : Base_Task_Definition)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_Task_Definition_Element
    (Self : Base_Task_Definition)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Task_Definition)
      return Boolean;

   type Task_Definition is
     new Base_Task_Definition
       and Program.Elements.Task_Definitions.Task_Definition_Text
     with record
        Private_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Task_Definition_Text
    (Self : aliased in out Task_Definition)
      return Program.Elements.Task_Definitions.Task_Definition_Text_Access;

   overriding function Private_Token
    (Self : Task_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Task_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Task_Definition is
     new Base_Task_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Task_Definition_Text
    (Self : aliased in out Implicit_Task_Definition)
      return Program.Elements.Task_Definitions.Task_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Task_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Task_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Task_Definition)
      return Boolean;

end Program.Nodes.Task_Definitions;
