--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Iterators;
with Program.Elements.Defining_Identifiers;
with Program.Interpretations;
with Program.Node_Symbols;
with Program.Type_Resolvers;

package body Program.Resolvers.Basic is

   ----------------------
   -- Enumeration_Type --
   ----------------------

   overriding procedure Enumeration_Type
     (Self    : in out Visitor;
      Element : not null Program.Elements.Enumeration_Types
        .Enumeration_Type_Access) is
   begin
      Self.Env.Create_Enumeration_Type
        (Symbol => Program.Node_Symbols.Get_Symbol (Self.Type_Name),
         Name   => Self.Type_Name);
      Self.Type_View := Self.Env.Latest_View;
      Self.Visit_Each_Child (Element);
   end Enumeration_Type;

   ---------------------------
   -- Exception_Declaration --
   ---------------------------

   overriding procedure Exception_Declaration
     (Self    : in out Visitor;
      Element : not null Program.Elements.Exception_Declarations
      .Exception_Declaration_Access)
   is
      Name : constant Program.Elements.Defining_Identifiers
        .Defining_Identifier_Access :=
          Element.Names.To_Defining_Identifier (1);
   begin
      Self.Env.Create_Exception
        (Symbol => Program.Node_Symbols.Get_Symbol (Name),
         Name   => Name.To_Defining_Name);
   end Exception_Declaration;

   -------------------------
   -- Floating_Point_Type --
   -------------------------

   overriding procedure Floating_Point_Type
     (Self    : in out Visitor;
      Element : not null Program.Elements.Floating_Point_Types
        .Floating_Point_Type_Access)
   is
      pragma Unreferenced (Element);
   begin
      Self.Env.Create_Float_Point_Type
        (Symbol => Program.Node_Symbols.Get_Symbol (Self.Type_Name),
         Name   => Self.Type_Name);
      Self.Env.Leave_Declarative_Region;
   end Floating_Point_Type;

   -------------------------
   -- Subtype_Declaration --
   -------------------------

   overriding procedure Subtype_Declaration
     (Self    : in out Visitor;
      Element : not null Program.Elements.Subtype_Declarations
        .Subtype_Declaration_Access)
   is
      Sets : aliased Program.Interpretations.Context (Self.Env);

      Subtype_Name : constant
        Program.Elements.Defining_Names.Defining_Name_Access :=
       Element.Name.To_Defining_Name;

      Type_View : Program.Visibility.View;

      Has_Constraint : constant Boolean :=
        Element.Subtype_Indication.Constraint.Assigned;
   begin
      Program.Type_Resolvers.Resolve_Type
        (Element.Subtype_Indication.Subtype_Mark,
         Self.Env,
         Self.Setter,
         Sets'Unchecked_Access,
         Type_View);

      Self.Env.Create_Subtype
        (Symbol      => Program.Node_Symbols.Get_Symbol (Subtype_Name),
         Name           => Subtype_Name,
         Subtype_Mark   => Type_View,
         Has_Constraint => Has_Constraint);

      if Has_Constraint then
         Element.Subtype_Indication.Constraint.Visit (Self);
      end if;

      for Aspect in Element.Aspects.Each_Element loop
         Aspect.Element.Visit (Self);
      end loop;

      Self.Env.Leave_Declarative_Region;
   end Subtype_Declaration;

   ----------------------
   -- Type_Declaration --
   ----------------------

   overriding procedure Type_Declaration
     (Self    : in out Visitor;
      Element : not null Program.Elements.Type_Declarations
        .Type_Declaration_Access) is
   begin
      Self.Type_Name :=
        Program.Elements.Defining_Names.Defining_Name_Access (Element.Name);
      Self.Discriminants := Element.Discriminant_Part;

      Self.Visit_Each_Child (Element);

      Self.Type_Name := null;
      Self.Discriminants := null;
   end Type_Declaration;

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

end Program.Resolvers.Basic;
