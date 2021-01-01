--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Defining_Names;
with Program.Elements.Identifiers;
with Program.Node_Symbols;
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

   ----------------
   -- Identifier --
   ----------------

   overriding procedure Identifier
    (Self    : in out Visitor;
     Element : not null Program.Elements.Identifiers.Identifier_Access)
   is
      Def    : Program.Elements.Defining_Names.Defining_Name_Access;
      Symbol : constant Program.Symbols.Symbol :=
        Program.Node_Symbols.Get_Symbol (Element);
      Views  : constant Program.Visibility.View_Array :=
        Self.Env.Immediate_Visible (Symbol);
   begin
      Self.View := Views (1);
      Def := Program.Visibility.Name (Self.View);
      Self.Setter.Set_Corresponding_Defining_Name (Element.all'Access, Def);
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

end Program.Type_Resolvers;
