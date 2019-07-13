--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Element_Vectors;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;

package Program.Elements.Task_Definitions is

   pragma Pure (Program.Elements.Task_Definitions);

   type Task_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Task_Definition_Access is access all Task_Definition'Class
     with Storage_Size => 0;

   not overriding function Visible_Declarations
    (Self : Task_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Private_Declarations
    (Self : Task_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function End_Name
    (Self : Task_Definition)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   type Task_Definition_Text is limited interface;

   type Task_Definition_Text_Access is access all Task_Definition_Text'Class
     with Storage_Size => 0;

   not overriding function To_Task_Definition_Text
    (Self : aliased in out Task_Definition)
      return Task_Definition_Text_Access is abstract;

   not overriding function Private_Token
    (Self : Task_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : Task_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Task_Definitions;
