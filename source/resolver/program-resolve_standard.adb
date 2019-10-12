--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Visitors;
with Program.Elements.Package_Declarations;
with Program.Symbols;
with Program.Visibility;

procedure Program.Resolve_Standard
  (Unit : not null Program.Compilation_Units.Compilation_Unit_Access)
is
   package Visitors is
      type Visitor
        (Env : not null access Program.Visibility.Context)
      is new Program.Element_Visitors.Element_Visitor with record
         null;
      end record;

      procedure Visit_Each_Child
        (Self    : in out Visitor;
         Element : access Program.Elements.Element'Class);

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

   end Visitors;

   package body Visitors is

      -------------------------
      -- Package_Declaration --
      -------------------------

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access) is
      begin
         Self.Env.Create_Package
           (Symbol => Program.Symbols.Standard,
            Name   => Element.Name);
         Self.Visit_Each_Child (Element);
      end Package_Declaration;

      ----------------------
      -- Visit_Each_Child --
      ----------------------

      procedure Visit_Each_Child
        (Self    : in out Visitor;
         Element : access Program.Elements.Element'Class) is
      begin
         for Cursor in Element.Each_Child loop
            Cursor.Element.Visit (Self);
         end loop;
      end Visit_Each_Child;

   end Visitors;

   Env : aliased Program.Visibility.Context;

   Visitor : Visitors.Visitor (Env'Access);
   Root    : constant Program.Elements.Element_Access := Unit.Unit_Declaration;
begin
   Env.Create_Empty_Context;
   Root.Visit (Visitor);
end Program.Resolve_Standard;
