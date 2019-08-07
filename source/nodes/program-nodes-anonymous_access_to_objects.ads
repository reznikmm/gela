--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;
with Program.Elements.Anonymous_Access_To_Objects;
with Program.Element_Visitors;

package Program.Nodes.Anonymous_Access_To_Objects is

   pragma Preelaborate;

   type Anonymous_Access_To_Object is
     new Program.Nodes.Node
         and Program.Elements.Anonymous_Access_To_Objects
           .Anonymous_Access_To_Object
         and Program.Elements.Anonymous_Access_To_Objects
           .Anonymous_Access_To_Object_Text
     with private;

   function Create
    (Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access)
      return Anonymous_Access_To_Object;

   type Implicit_Anonymous_Access_To_Object is
     new Program.Nodes.Node
         and Program.Elements.Anonymous_Access_To_Objects
           .Anonymous_Access_To_Object
     with private;

   function Create
    (Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_All              : Boolean := False;
     Has_Constant         : Boolean := False)
      return Implicit_Anonymous_Access_To_Object
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Anonymous_Access_To_Object is
     abstract new Program.Nodes.Node
       and Program.Elements.Anonymous_Access_To_Objects
         .Anonymous_Access_To_Object
     with record
        Subtype_Indication : not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Anonymous_Access_To_Object'Class);

   overriding procedure Visit
    (Self    : not null access Base_Anonymous_Access_To_Object;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subtype_Indication
    (Self : Base_Anonymous_Access_To_Object)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;

   overriding function Is_Anonymous_Access_To_Object
    (Self : Base_Anonymous_Access_To_Object)
      return Boolean;

   overriding function Is_Anonymous_Access_Definition
    (Self : Base_Anonymous_Access_To_Object)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Anonymous_Access_To_Object)
      return Boolean;

   type Anonymous_Access_To_Object is
     new Base_Anonymous_Access_To_Object
       and Program.Elements.Anonymous_Access_To_Objects
         .Anonymous_Access_To_Object_Text
     with record
        Not_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Access_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        All_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Constant_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Anonymous_Access_To_Object_Text
    (Self : aliased in out Anonymous_Access_To_Object)
      return Program.Elements.Anonymous_Access_To_Objects
          .Anonymous_Access_To_Object_Text_Access;

   overriding function Not_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Access_Token
    (Self : Anonymous_Access_To_Object)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function All_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Constant_Token
    (Self : Anonymous_Access_To_Object)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null
    (Self : Anonymous_Access_To_Object)
      return Boolean;

   overriding function Has_All
    (Self : Anonymous_Access_To_Object)
      return Boolean;

   overriding function Has_Constant
    (Self : Anonymous_Access_To_Object)
      return Boolean;

   type Implicit_Anonymous_Access_To_Object is
     new Base_Anonymous_Access_To_Object
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
        Has_All              : Boolean;
        Has_Constant         : Boolean;
     end record;

   overriding function To_Anonymous_Access_To_Object_Text
    (Self : aliased in out Implicit_Anonymous_Access_To_Object)
      return Program.Elements.Anonymous_Access_To_Objects
          .Anonymous_Access_To_Object_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean;

   overriding function Has_All
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean;

   overriding function Has_Constant
    (Self : Implicit_Anonymous_Access_To_Object)
      return Boolean;

end Program.Nodes.Anonymous_Access_To_Objects;
