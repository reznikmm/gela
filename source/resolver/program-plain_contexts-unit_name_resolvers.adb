--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Elements.Package_Declarations;
with Program.Node_Symbols;
with Program.Safe_Element_Visitors;

package body Program.Plain_Contexts.Unit_Name_Resolvers is

   type Getter is new Program.Safe_Element_Visitors.Safe_Element_Visitor with
   record
      Value : Program.Elements.Defining_Names.Defining_Name_Access;
   end record;

   overriding procedure Defining_Identifier
     (Self    : in out Getter;
      Element : not null Program.Elements.Defining_Identifiers
        .Defining_Identifier_Access);

   overriding procedure Package_Declaration
     (Self    : in out Getter;
      Element : not null Program.Elements.Package_Declarations
        .Package_Declaration_Access);

   -------------------------
   -- Defining_Identifier --
   -------------------------

   overriding procedure Defining_Identifier
     (Self    : in out Getter;
      Element : not null Program.Elements.Defining_Identifiers
        .Defining_Identifier_Access) is
   begin
      Self.Value := Element.To_Defining_Name;
   end Defining_Identifier;

   -------------------------
   -- Package_Declaration --
   -------------------------

   overriding procedure Package_Declaration
     (Self    : in out Getter;
      Element : not null Program.Elements.Package_Declarations
        .Package_Declaration_Access) is
   begin
      Element.Name.Visit (Self);
   end Package_Declaration;

   ------------------------
   -- Resolve_Identifier --
   ------------------------

   overriding procedure Resolve_Identifier
     (Self   : Unit_Name_Resolver;
      Name   : not null Program.Elements.Identifiers.Identifier_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
   is
      V    : Getter;
      List : Program.Symbol_Lists.Symbol_List;
      Unit : Program.Compilation_Units.Compilation_Unit_Access;
   begin
      Program.Node_Symbols.Name_Symbol
        (Self.Lists.all, Name.To_Expression, List);

      if Self.Declarations.Map.Contains (List) then
         Unit := Self.Declarations.Map (List).Unit;
      elsif Self.Bodies.Map.Contains (List) then
         Unit := Self.Bodies.Map (List).Unit;
      else
         Self.Errors.No_Such_Unit (Self.Lists.Symbol_List_Text (List));
         return;
      end if;

      Unit.Unit_Declaration.Visit (V);

      Setter.Set_Corresponding_Defining_Name
        (Name.To_Element, V.Value);
   end Resolve_Identifier;

end Program.Plain_Contexts.Unit_Name_Resolvers;
