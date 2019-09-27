--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Visitors;
with Program.Elements;
with Program.Elements.Package_Declarations;
with Program.Symbols;
with Program.Elements.Defining_Names;

package body Program.Resolvers is

   package Visitors is
      type Visitor
        (Env : not null access Program.Visibility.Context)
      is new Program.Element_Visitors.Element_Visitor with record
         null;
      end record;

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
         .Package_Declaration_Access);

   end Visitors;

   -------------------
   -- Resolve_Names --
   -------------------

   procedure Resolve_Names
     (Env  : aliased in out Program.Visibility.Context;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access)
   is
      Element : constant Program.Elements.Element_Access :=
        Unit.Unit_Declaration;
      Visitor : Visitors.Visitor (Env'Access);
   begin
      Element.Visit (Visitor);
   end Resolve_Names;

   --------------
   -- Visitors --
   --------------

   package body Visitors is

      -------------------------
      -- Package_Declaration --
      -------------------------

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null
           Program.Elements.Package_Declarations.Package_Declaration_Access)
      is
         Name   : constant
           Program.Elements.Defining_Names.Defining_Name_Access :=
             Element.Name;
         Symbol : constant Program.Symbols.Symbol := 0;
      begin
         Self.Env.Create_Package
           (Symbol => Symbol,
            Name   => Name);
      end Package_Declaration;

   end Visitors;

end Program.Resolvers;
