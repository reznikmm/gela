--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------
--
--  Common type for Standard and full resolver

with Program.Element_Visitors;
with Program.Elements.Defining_Names;
with Program.Elements.Definitions;
with Program.Elements.Enumeration_Types;
with Program.Elements.Exception_Declarations;
with Program.Elements.Floating_Point_Types;
with Program.Elements.Subtype_Declarations;
with Program.Elements.Type_Declarations;
with Program.Visibility;
with Program.Cross_Reference_Updaters;

package Program.Resolvers.Basic is
   pragma Preelaborate;

   type Visitor
     (Env    : not null Program.Visibility.Context_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
   is new Program.Element_Visitors.Element_Visitor with record
      Discriminants : Program.Elements.Definitions.Definition_Access;
      Type_Name     : Program.Elements.Defining_Names.Defining_Name_Access;
      Type_View     : Program.Visibility.View;
   end record;

   procedure Visit_Each_Child
     (Self    : in out Visitor;
      Element : access Program.Elements.Element'Class);

   overriding procedure Enumeration_Type
     (Self    : in out Visitor;
      Element : not null Program.Elements.Enumeration_Types
        .Enumeration_Type_Access);

   overriding procedure Exception_Declaration
     (Self    : in out Visitor;
      Element : not null Program.Elements.Exception_Declarations
        .Exception_Declaration_Access);

   overriding procedure Floating_Point_Type
     (Self    : in out Visitor;
      Element : not null Program.Elements.Floating_Point_Types
        .Floating_Point_Type_Access);

   overriding procedure Subtype_Declaration
     (Self    : in out Visitor;
      Element : not null Program.Elements.Subtype_Declarations
        .Subtype_Declaration_Access);

   overriding procedure Type_Declaration
     (Self    : in out Visitor;
      Element : not null Program.Elements.Type_Declarations
        .Type_Declaration_Access);

end Program.Resolvers.Basic;
