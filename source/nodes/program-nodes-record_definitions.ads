--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Record_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Record_Definitions is

   pragma Preelaborate;

   type Record_Definition is
     new Program.Nodes.Node
         and Program.Elements.Record_Definitions.Record_Definition
         and Program.Elements.Record_Definitions.Record_Definition_Text
     with private;

   function Create
    (Record_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components     : not null Program.Element_Vectors.Element_Vector_Access;
     End_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Record_Token_2 : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Record_Definition;

   type Implicit_Record_Definition is
     new Program.Nodes.Node
         and Program.Elements.Record_Definitions.Record_Definition
     with private;

   function Create
    (Components           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Record_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Record_Definitions.Record_Definition
     with record
        Components : not null Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Record_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Record_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Components
    (Self : Base_Record_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Record_Definition
    (Self : Base_Record_Definition)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Record_Definition)
      return Boolean;

   type Record_Definition is
     new Base_Record_Definition
       and Program.Elements.Record_Definitions.Record_Definition_Text
     with record
        Record_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Record_Token_2 : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Record_Definition_Text
    (Self : aliased in out Record_Definition)
      return Program.Elements.Record_Definitions.Record_Definition_Text_Access;

   overriding function Record_Token
    (Self : Record_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Record_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Record_Token_2
    (Self : Record_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Record_Definition is
     new Base_Record_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Record_Definition_Text
    (Self : aliased in out Implicit_Record_Definition)
      return Program.Elements.Record_Definitions.Record_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Definition)
      return Boolean;

end Program.Nodes.Record_Definitions;
