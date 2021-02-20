--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Elements.Defining_Names;
with Program.Elements.Identifiers;
with Program.Elements.Subtype_Indications;
with Program.Node_Symbols;
with Program.Nodes.Proxy_Calls;
with Program.Safe_Element_Visitors;
with Program.Symbols;

package body Program.Type_Resolvers is

   type Visitor
     (Env    : not null Program.Visibility.Context_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
          is new Program.Safe_Element_Visitors.Safe_Element_Visitor
   with record
      Has_View : Boolean := False;
      View     : Program.Visibility.View;
   end record;

   overriding procedure Identifier
    (Self    : in out Visitor;
     Element : not null Program.Elements.Identifiers.Identifier_Access);

   overriding procedure Subtype_Indication
    (Self    : in out Visitor;
     Element : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access);

   ----------------
   -- Identifier --
   ----------------

   overriding procedure Identifier
    (Self    : in out Visitor;
     Element : not null Program.Elements.Identifiers.Identifier_Access)
   is
      use type Program.Visibility.View_Cursor;
      Def    : Program.Elements.Defining_Names.Defining_Name_Access;
      Symbol : constant Program.Symbols.Symbol :=
        Program.Node_Symbols.Get_Symbol (Element);
   begin
      for View in Self.Env.Immediate_Visible (Symbol) loop
         Self.View := +View;
         Def := Program.Visibility.Name (Self.View);
         Self.Setter.Set_Corresponding_Defining_Name (Element.all'Access, Def);

         return;
      end loop;
   end Identifier;


   ------------------
   -- Resolve_Type --
   ------------------

   procedure Resolve_Type
     (Element : Program.Elements.Expressions.Expression_Access;
      Context : not null Program.Visibility.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access;
      Value : out Program.Visibility.View)
   is
      V : Visitor (Context, Setter);
   begin
      V.Visit (Element);
      Value := V.View;
   end Resolve_Type;

   -----------------------------
   -- Resolve_Type_Definition --
   -----------------------------

   procedure Resolve_Type_Definition
     (Element : Program.Elements.Element_Access;
      Context : not null Program.Visibility.Context_Access;
      Setter  : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Value   : out Program.Visibility.View)
   is
      V : Visitor (Context, Setter);
   begin
      V.Visit (Element);
      Value := V.View;
   end Resolve_Type_Definition;

   ------------------------
   -- Subtype_Indication --
   ------------------------

   overriding procedure Subtype_Indication
    (Self    : in out Visitor;
     Element : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access)
   is
      Constr : constant Program.Elements.Constraints.Constraint_Access :=
        Element.Constraint;
   begin
      Self.Visit (Element.Subtype_Mark);

      if Constr.Assigned
        and then Self.View.Kind in Program.Visibility.Array_Type_View
        and then Constr.Is_Discriminant_Constraint
        and then Constr.all in Program.Nodes.Proxy_Calls.Proxy_Call'Class
      then
         Program.Nodes.Proxy_Calls.Proxy_Call'Class (Constr.all)
           .Turn_To_Index_Constraint;
      end if;
   end Subtype_Indication;

end Program.Type_Resolvers;
