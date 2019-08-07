--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Protected_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Protected_Definitions is

   pragma Pure (Program.Nodes.Protected_Definitions);

   type Protected_Definition is
     new Program.Nodes.Node
         and Program.Elements.Protected_Definitions.Protected_Definition
         and Program.Elements.Protected_Definitions.Protected_Definition_Text
     with private;

   function Create
    (Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access)
      return Protected_Definition;

   type Implicit_Protected_Definition is
     new Program.Nodes.Node
         and Program.Elements.Protected_Definitions.Protected_Definition
     with private;

   function Create
    (Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Protected_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Protected_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Protected_Definitions.Protected_Definition
     with record
        Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
        Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
        End_Name             : Program.Elements.Identifiers.Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Protected_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Protected_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Visible_Declarations
    (Self : Base_Protected_Definition)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Private_Declarations
    (Self : Base_Protected_Definition)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function End_Name
    (Self : Base_Protected_Definition)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_Protected_Definition
    (Self : Base_Protected_Definition)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Protected_Definition)
      return Boolean;

   type Protected_Definition is
     new Base_Protected_Definition
       and Program.Elements.Protected_Definitions.Protected_Definition_Text
     with record
        Private_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Protected_Definition_Text
    (Self : aliased in out Protected_Definition)
      return Program.Elements.Protected_Definitions
          .Protected_Definition_Text_Access;

   overriding function Private_Token
    (Self : Protected_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Protected_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Protected_Definition is
     new Base_Protected_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Protected_Definition_Text
    (Self : aliased in out Implicit_Protected_Definition)
      return Program.Elements.Protected_Definitions
          .Protected_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Protected_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Protected_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Protected_Definition)
      return Boolean;

end Program.Nodes.Protected_Definitions;
