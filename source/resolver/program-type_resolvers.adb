--  SPDX-FileCopyrightText: 2020-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Constraints;
with Program.Elements.Defining_Names;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Discriminant_Associations;
with Program.Elements.Identifiers;
with Program.Elements.Subtype_Indications;
with Program.Complete_Contexts;
with Program.Node_Symbols;
with Program.Nodes.Proxy_Calls;
with Program.Resolvers.Name_In_Region;
with Program.Safe_Element_Visitors;
with Program.Symbols;

package body Program.Type_Resolvers is

   type Visitor
     (Env    : not null Program.Visibility.Context_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Sets   : not null Program.Interpretations.Context_Access)
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
      Sets    : not null Program.Interpretations.Context_Access;
      Value : out Program.Visibility.View)
   is
      V : Visitor (Context, Setter, Sets);
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
      Sets    : not null Program.Interpretations.Context_Access;
      Value   : out Program.Visibility.View)
   is
      V : Visitor (Context, Setter, Sets);
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

      if not Constr.Assigned then
         return;
      end if;

      if Self.View.Kind in Program.Visibility.Array_Type_View then
         if Constr.Is_Discriminant_Constraint
           and then Constr.all in Program.Nodes.Proxy_Calls.Proxy_Call'Class
         then
            --  Fix parser assumption for `F(X)` to make it index constraint
            Program.Nodes.Proxy_Calls.Proxy_Call'Class (Constr.all)
              .Turn_To_Index_Constraint;
         end if;

         --  ARM:3.6.1(4): each discrete_range shall resolve to be of the type
         --  of the corresponding index.

         declare
            List : constant Program.Visibility.View_Array :=
              Program.Visibility.Indexes (Self.View);

            Args : constant Program.Elements.Discrete_Ranges
                             .Discrete_Range_Vector_Access :=
                                Constr.To_Index_Constraint.Ranges;
         begin
            pragma Assert (List'Length = Args.Length);

            for J in Args.Each_Element loop
               Program.Complete_Contexts.Resolve_To_Expected_Type
                 (Sets    => Self.Sets,
                  Setter  => Self.Setter,
                  Expect  => Program.Visibility.First_Subtype (List (J.Index)),
                  Element => J.Element);
            end loop;
         end;

      elsif Self.View.Kind in Program.Visibility.Record_Type_View then
         pragma Assert (Constr.Is_Discriminant_Constraint);

         for J in
           Constr.To_Discriminant_Constraint.Discriminants.Each_Element
         loop
            declare
               Compon : Program.Visibility.View;
               Assoc  : constant Program.Elements.Discriminant_Associations
                 .Discriminant_Association_Access :=
                   J.Element.To_Discriminant_Association;

               Choices : constant Program.Elements.Identifiers
                 .Identifier_Vector_Access := Assoc.Selector_Names;
            begin
               pragma Assert (Choices.Length > 0);

               for K in Choices.Each_Element loop
                  Program.Resolvers.Name_In_Region.Resolve_Name
                    (Region => Self.View,
                     Name   => K.Element.To_Expression,
                     Setter => Self.Setter);

                  Compon := Self.Sets.Env.Get_Name_View (K.Element);

                  Program.Complete_Contexts.Resolve_To_Expected_Type
                    (Assoc.Discriminant_Value.To_Element,
                     Self.Sets,
                     Self.Setter,
                     Expect => Program.Visibility.Subtype_Mark (Compon));
               end loop;
            end;
         end loop;

      end if;
   end Subtype_Indication;

end Program.Type_Resolvers;
