--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Root_Types;
with Program.Element_Visitors;

package Program.Nodes.Root_Types is

   pragma Preelaborate;

   type Root_Type is
     new Program.Nodes.Node and Program.Elements.Root_Types.Root_Type
         and Program.Elements.Root_Types.Root_Type_Text
     with private;

   function Create return Root_Type;

   type Implicit_Root_Type is
     new Program.Nodes.Node and Program.Elements.Root_Types.Root_Type
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Root_Type
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Root_Type is
     abstract new Program.Nodes.Node and Program.Elements.Root_Types.Root_Type
     with null record;

   procedure Initialize (Self : aliased in out Base_Root_Type'Class);

   overriding procedure Visit
    (Self    : not null access Base_Root_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Root_Type (Self : Base_Root_Type) return Boolean;

   overriding function Is_Type_Definition
    (Self : Base_Root_Type)
      return Boolean;

   overriding function Is_Definition (Self : Base_Root_Type) return Boolean;

   type Root_Type is
     new Base_Root_Type and Program.Elements.Root_Types.Root_Type_Text
     with null record;

   overriding function To_Root_Type_Text
    (Self : aliased in out Root_Type)
      return Program.Elements.Root_Types.Root_Type_Text_Access;

   type Implicit_Root_Type is
     new Base_Root_Type
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Root_Type_Text
    (Self : aliased in out Implicit_Root_Type)
      return Program.Elements.Root_Types.Root_Type_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Root_Type)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Root_Type)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Root_Type)
      return Boolean;

end Program.Nodes.Root_Types;
