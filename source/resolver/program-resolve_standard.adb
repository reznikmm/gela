--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vector_Factories;
with Program.Element_Vectors;
with Program.Element_Visitors;
with Program.Elements.Component_Definitions;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Names;
with Program.Elements.Enumeration_Literal_Specifications;
with Program.Elements.Identifiers;
with Program.Elements.Package_Declarations;
with Program.Elements.Signed_Integer_Types;
with Program.Elements.Subtype_Declarations;
with Program.Elements.Subtype_Indications;
with Program.Elements.Unconstrained_Array_Types;
with Program.Implicit_Element_Factories;
with Program.Lexical_Elements;
with Program.Node_Symbols;
with Program.Plain_Lexical_Elements;
with Program.Predefined_Operators;
with Program.Symbol_Lists;
with Program.Symbols;
with Program.Visibility;
with Program.Resolvers.Basic;

procedure Program.Resolve_Standard
  (Unit    : not null Program.Compilation_Units.Compilation_Unit_Access;
   Context : aliased in out Program.Visibility.Context;
   Library : in out Program.Library_Environments.Library_Environment;
   Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle;
   Setter  : not null
     Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
is

   function To_Symbol
     (Name : access Program.Elements.Element'Class)
       return Program.Symbols.Symbol;

   Factory : Program.Implicit_Element_Factories.Element_Factory (Subpool);
   Vectors : Program.Element_Vector_Factories.Element_Vector_Factory (Subpool);

   package Visitors is
      type Visitor
      is new Program.Resolvers.Basic.Visitor with record
         Meta_Char : Program.Visibility.Meta_Character_Literal_Kind :=
           Program.Visibility.Meta_Character;
      end record;

      overriding procedure Enumeration_Literal_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification_Access);

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

      overriding procedure Signed_Integer_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Signed_Integer_Types
           .Signed_Integer_Type_Access);

      overriding procedure Subtype_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Subtype_Declarations
           .Subtype_Declaration_Access);

      overriding procedure Unconstrained_Array_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Unconstrained_Array_Types
           .Unconstrained_Array_Type_Access);

   end Visitors;

   package body Visitors is

      ---------------------------------------
      -- Enumeration_Literal_Specification --
      ---------------------------------------

      overriding procedure Enumeration_Literal_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification_Access)
      is
         Symbol : constant Program.Symbols.Symbol :=
           Program.Node_Symbols.Get_Symbol (Element.Name);
      begin
         if Program.Symbols.Is_Character_Literal (Symbol) then
            Self.Env.Create_Character_Literal
              (Symbol,
               Element.Name,
               Self.Meta_Char,
               Self.Type_View);

            if Self.Meta_Char not in Visibility.Meta_Wide_Wide_Character then
               Self.Meta_Char :=
                 Visibility.Meta_Character_Literal_Kind'Succ (Self.Meta_Char);
            end if;
         else
            Self.Env.Create_Enumeration_Literal
              (Symbol,
               Element.Name,
               Self.Type_View);
         end if;
      end Enumeration_Literal_Specification;

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

      -------------------------
      -- Signed_Integer_Type --
      -------------------------

      overriding procedure Signed_Integer_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Signed_Integer_Types
           .Signed_Integer_Type_Access)
      is
         pragma Unreferenced (Element);
         Type_View : Program.Visibility.View;
         Ignore    : Program.Element_Vectors.Element_Vector_Access;
      begin
         Self.Env.Create_Signed_Integer_Type
           (Symbol => Program.Node_Symbols.Get_Symbol (Self.Type_Name),
            Name   => Self.Type_Name);

         Type_View := Self.Env.Latest_View;
         Self.Env.Leave_Declarative_Region;

         Program.Predefined_Operators.Create_Operators_For_Integer
           (Self      => Self.Env.all,
            Type_View => Type_View,
            Setter    => Setter,
            Factory   => Factory,
            Vectors   => Vectors,
            Result    => Ignore);  --  FIXME: keep result in AST
      end Signed_Integer_Type;

      -------------------------
      -- Subtype_Declaration --
      -------------------------

      overriding procedure Subtype_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Subtype_Declarations
           .Subtype_Declaration_Access)
      is
         use type Program.Visibility.View_Cursor;

         Subtype_Name : constant
           Program.Elements.Defining_Names.Defining_Name_Access :=
             Element.Name.To_Defining_Name;

         Subtype_Mark_Symbol : constant Program.Symbols.Symbol :=
           To_Symbol (Element.Subtype_Indication.Subtype_Mark);

      begin
         for J in Self.Env.Immediate_Visible (Subtype_Mark_Symbol) loop
            Self.Env.Create_Subtype
              (Symbol       => Program.Node_Symbols.Get_Symbol (Subtype_Name),
               Name         => Subtype_Name,
               Subtype_Mark => +J,
               Has_Constraint => Element
               .Subtype_Indication.Constraint.Assigned);

            Self.Visit_Each_Child (Element);
            Self.Env.Leave_Declarative_Region;

            return;
         end loop;

         raise Program_Error;
      end Subtype_Declaration;

      ------------------------------
      -- Unconstrained_Array_Type --
      ------------------------------

      overriding procedure Unconstrained_Array_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Unconstrained_Array_Types
           .Unconstrained_Array_Type_Access)
      is
         use type Program.Visibility.View_Cursor;

         Index_Symbol : constant Program.Symbols.Symbol :=
           To_Symbol (Element.Index_Subtypes.Element (1));
         Component_Symbol : constant Program.Symbols.Symbol :=
           To_Symbol (Element.Component_Definition);
         Type_View        : Program.Visibility.View;
         Ignore : Program.Element_Vectors.Element_Vector_Access;
      begin
         for Index_View in Self.Env.Immediate_Visible (Index_Symbol) loop
            for Component in Self.Env.Immediate_Visible (Component_Symbol) loop
               Self.Env.Create_Array_Type
                 (Symbol  => Program.Node_Symbols.Get_Symbol (Self.Type_Name),
                  Name    => Self.Type_Name,
                  Indexes => (1 => +Index_View),
                  Component => +Component);

               Type_View := Self.Env.Latest_View;
               Self.Env.Leave_Declarative_Region;

               Program.Predefined_Operators.Create_Operators_For_Array
                 (Self      => Self.Env.all,
                  Type_View => Type_View,
                  Setter    => Setter,
                  Factory   => Factory,
                  Vectors   => Vectors,
                  Result    => Ignore);  --  FIXME: keep result in AST

               return;
            end loop;
         end loop;

         raise Program_Error;
      end Unconstrained_Array_Type;

   end Visitors;

   function To_Symbol
     (Name : access Program.Elements.Element'Class)
      return Program.Symbols.Symbol
   is
      type Getter is new Program.Element_Visitors.Element_Visitor with record
         Result : Program.Symbols.Symbol := Program.Symbols.No_Symbol;
      end record;

      overriding procedure Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

      overriding procedure Component_Definition
        (Self    : in out Getter;
         Element : not null Program.Elements.Component_Definitions
           .Component_Definition_Access);

      overriding procedure Subtype_Indication
        (Self    : in out Getter;
         Element : not null Program.Elements.Subtype_Indications
           .Subtype_Indication_Access);

      --------------------------
      -- Component_Definition --
      --------------------------

      overriding procedure Component_Definition
        (Self    : in out Getter;
         Element : not null Program.Elements.Component_Definitions
           .Component_Definition_Access)
      is
      begin
         Element.Subtype_Indication.Visit (Self);
      end Component_Definition;

      ----------------
      -- Identifier --
      ----------------

      overriding procedure Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Identifiers.Identifier_Access)
      is
         Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
           Element.To_Identifier_Text.Identifier_Token;
      begin
         Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
           (Token.all).Symbol;
      end Identifier;

      ------------------------
      -- Subtype_Indication --
      ------------------------

      overriding procedure Subtype_Indication
        (Self    : in out Getter;
         Element : not null Program.Elements.Subtype_Indications
           .Subtype_Indication_Access)
      is
      begin
         Element.Subtype_Mark.Visit (Self);
      end Subtype_Indication;

      G : Getter;
   begin
      Name.Visit (G);
      pragma Assert (G.Result not in Program.Symbols.No_Symbol);

      return G.Result;
   end To_Symbol;


   Visitor : Visitors.Visitor (Context'Unchecked_Access, Setter);
   Root    : constant Program.Elements.Element_Access := Unit.Unit_Declaration;
begin
   Context.Create_Empty_Context;
   Root.Visit (Visitor);

   Library.Put_Public_View
     (Program.Symbol_Lists.Empty_Symbol_List,  --  Means Standard root package
      Context.Create_Snapshot);
end Program.Resolve_Standard;
