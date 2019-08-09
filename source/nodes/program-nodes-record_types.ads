--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Definitions;
with Program.Elements.Record_Types;
with Program.Element_Visitors;

package Program.Nodes.Record_Types is

   pragma Preelaborate;

   type Record_Type is
     new Program.Nodes.Node and Program.Elements.Record_Types.Record_Type
         and Program.Elements.Record_Types.Record_Type_Text
     with private;

   function Create
    (Abstract_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Tagged_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Limited_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Definition : not null Program.Elements.Definitions
         .Definition_Access)
      return Record_Type;

   type Implicit_Record_Type is
     new Program.Nodes.Node and Program.Elements.Record_Types.Record_Type
     with private;

   function Create
    (Record_Definition    : not null Program.Elements.Definitions
         .Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Record_Type is
     abstract new Program.Nodes.Node
       and Program.Elements.Record_Types.Record_Type
     with record
        Record_Definition : not null Program.Elements.Definitions
          .Definition_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Record_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Record_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Record_Definition
    (Self : Base_Record_Type)
      return not null Program.Elements.Definitions.Definition_Access;

   overriding function Is_Record_Type_Element
    (Self : Base_Record_Type)
      return Boolean;

   overriding function Is_Type_Definition_Element
    (Self : Base_Record_Type)
      return Boolean;

   overriding function Is_Definition_Element
    (Self : Base_Record_Type)
      return Boolean;

   type Record_Type is
     new Base_Record_Type and Program.Elements.Record_Types.Record_Type_Text
     with record
        Abstract_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Tagged_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Limited_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Record_Type_Text
    (Self : aliased in out Record_Type)
      return Program.Elements.Record_Types.Record_Type_Text_Access;

   overriding function Abstract_Token
    (Self : Record_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Tagged_Token
    (Self : Record_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Limited_Token
    (Self : Record_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Record_Type is
     new Base_Record_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Record_Type_Text
    (Self : aliased in out Implicit_Record_Type)
      return Program.Elements.Record_Types.Record_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Type)
      return Boolean;

end Program.Nodes.Record_Types;
