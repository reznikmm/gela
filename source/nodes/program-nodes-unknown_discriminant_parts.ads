--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Unknown_Discriminant_Parts;
with Program.Element_Visitors;

package Program.Nodes.Unknown_Discriminant_Parts is

   pragma Preelaborate;

   type Unknown_Discriminant_Part is
     new Program.Nodes.Node
         and Program.Elements.Unknown_Discriminant_Parts
           .Unknown_Discriminant_Part
         and Program.Elements.Unknown_Discriminant_Parts
           .Unknown_Discriminant_Part_Text
     with private;

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Box_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Unknown_Discriminant_Part;

   type Implicit_Unknown_Discriminant_Part is
     new Program.Nodes.Node
         and Program.Elements.Unknown_Discriminant_Parts
           .Unknown_Discriminant_Part
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Unknown_Discriminant_Part
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Unknown_Discriminant_Part is
     abstract new Program.Nodes.Node
       and Program.Elements.Unknown_Discriminant_Parts
         .Unknown_Discriminant_Part
     with null record;

   procedure Initialize
    (Self : aliased in out Base_Unknown_Discriminant_Part'Class);

   overriding procedure Visit
    (Self    : not null access Base_Unknown_Discriminant_Part;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Unknown_Discriminant_Part
    (Self : Base_Unknown_Discriminant_Part)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Unknown_Discriminant_Part)
      return Boolean;

   type Unknown_Discriminant_Part is
     new Base_Unknown_Discriminant_Part
       and Program.Elements.Unknown_Discriminant_Parts
         .Unknown_Discriminant_Part_Text
     with record
        Left_Bracket_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Box_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Unknown_Discriminant_Part_Text
    (Self : aliased in out Unknown_Discriminant_Part)
      return Program.Elements.Unknown_Discriminant_Parts
          .Unknown_Discriminant_Part_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Unknown_Discriminant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Box_Token
    (Self : Unknown_Discriminant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Unknown_Discriminant_Part)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Unknown_Discriminant_Part is
     new Base_Unknown_Discriminant_Part
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Unknown_Discriminant_Part_Text
    (Self : aliased in out Implicit_Unknown_Discriminant_Part)
      return Program.Elements.Unknown_Discriminant_Parts
          .Unknown_Discriminant_Part_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Unknown_Discriminant_Part)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Unknown_Discriminant_Part)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Unknown_Discriminant_Part)
      return Boolean;

end Program.Nodes.Unknown_Discriminant_Parts;
