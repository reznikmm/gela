--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Iterators;
with Program.Element_Visitors;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Names;
with Program.Elements.Identifiers;
with Program.Elements.Package_Declarations;
with Program.Elements.Procedure_Body_Declarations;
with Program.Elements.Subtype_Declarations;
with Program.Elements.Use_Clauses;
with Program.Elements.With_Clauses;
with Program.Elements;
with Program.Lexical_Elements;
with Program.Node_Symbols;
with Program.Safe_Element_Visitors;
with Program.Symbols;
with Program.Type_Resolvers;

package body Program.Resolvers is

   package Visitors is

      type Snapshot_Registry is limited interface;

      type Snapshot_Registry_Access is access all Snapshot_Registry'Class
        with Storage_Size => 0;

      not overriding procedure Put_Public_View
        (Self  : in out Snapshot_Registry;
         Name  : Program.Symbols.Symbol;
         Value : Program.Visibility.Snapshot_Access) is abstract;

      not overriding procedure Push
        (Self  : in out Snapshot_Registry;
         Name  : Program.Symbols.Symbol) is abstract;

      not overriding procedure Pop
        (Self  : in out Snapshot_Registry) is abstract;

      type Visitor
        (Env    : not null Program.Visibility.Context_Access;
         Unit   : not null Program.Elements.Element_Access;
         Setter : not null
           Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
      is new Program.Element_Visitors.Element_Visitor with record
--    is new Program.Safe_Element_Visitors.Safe_Element_Visitor with record
         Snapshot_Registry : Snapshot_Registry_Access;
      end record;

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

      overriding procedure Subtype_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Subtype_Declarations
           .Subtype_Declaration_Access);

   end Visitors;

   package Environment_Level is

      type Visitor
        (Unit_Name_Resolver : not null
           Program.Simple_Resolvers.Simple_Resolver_Access;
         Setter             : not null
           Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
      is new Program.Safe_Element_Visitors.Safe_Element_Visitor
        with null record;

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access);

      overriding procedure Use_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.Use_Clauses.Use_Clause_Access);

      overriding procedure With_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.With_Clauses.With_Clause_Access);

   end Environment_Level;

   package body Environment_Level is

      -------------------------
      -- Package_Declaration --
      -------------------------

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access)
      is
      begin
         --  Find parent name and resolve it
         null;
      end Package_Declaration;

      --------------------------------
      -- Procedure_Body_Declaration --
      --------------------------------

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Body_Declarations
         .Procedure_Body_Declaration_Access) is
      begin
         null;  --  FIXME: Find parent name and resolve it
      end Procedure_Body_Declaration;

      ----------------
      -- Use_Clause --
      ----------------

      overriding procedure Use_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.Use_Clauses.Use_Clause_Access) is
      begin
         pragma Assert (not Element.Has_Type);

         for Name in Element.Clause_Names.Each_Element loop
            Self.Unit_Name_Resolver.Resolve
              (Name.Element.To_Expression, Self.Setter);
         end loop;
      end Use_Clause;

      -----------------
      -- With_Clause --
      -----------------

      overriding procedure With_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.With_Clauses.With_Clause_Access)
      is
      begin
         pragma Assert (not Element.Has_Limited);

         for Name in Element.Clause_Names.Each_Element loop
            Self.Unit_Name_Resolver.Resolve
              (Name.Element.To_Expression, Self.Setter);
         end loop;
      end With_Clause;

   end Environment_Level;

   package Global_Snapshots is

      type Library_Environment_Access is
        access all Program.Library_Environments.Library_Environment'Class
          with Storage_Size => 0;

      type Snapshot_Registry
        (Lists : not null Program.Symbol_Lists.Symbol_List_Table_Access;
         Lib   : not null Library_Environment_Access) is
        new Visitors.Snapshot_Registry with
      record
         Parent : Program.Symbol_Lists.Symbol_List;
      end record;

      overriding procedure Put_Public_View
        (Self  : in out Snapshot_Registry;
         Name  : Program.Symbols.Symbol;
         Value : Program.Visibility.Snapshot_Access);

      overriding procedure Push
        (Self  : in out Snapshot_Registry;
         Name  : Program.Symbols.Symbol);

      overriding procedure Pop
        (Self  : in out Snapshot_Registry);

   end Global_Snapshots;

   package body Global_Snapshots is

      overriding procedure Put_Public_View
        (Self  : in out Snapshot_Registry;
         Name  : Program.Symbols.Symbol;
         Value : Program.Visibility.Snapshot_Access)
      is
         List : Program.Symbol_Lists.Symbol_List;
      begin
         Self.Lists.Find_Or_Create (Self.Parent, Name, List);
         Self.Lib.Put_Public_View (List, Value);
      end Put_Public_View;

      overriding procedure Push
        (Self  : in out Snapshot_Registry;
         Name  : Program.Symbols.Symbol) is
      begin
         Self.Lists.Find_Or_Create (Self.Parent, Name, Self.Parent);
      end Push;

      overriding procedure Pop (Self  : in out Snapshot_Registry) is
      begin
         Self.Parent := Self.Lists.Prefix (Self.Parent);
      end Pop;

   end Global_Snapshots;

   -------------------
   -- Resolve_Names --
   -------------------

   procedure Resolve_Names
     (Unit    : not null Program.Compilation_Units.Compilation_Unit_Access;
      Unit_Name_Resolver : not null
        Program.Simple_Resolvers.Simple_Resolver_Access;
      Lists   : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Context : not null Program.Visibility.Context_Access;
      Library : in out Program.Library_Environments.Library_Environment;
      Setter  : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
   is

      procedure Create_Context (Unit_Name : Program.Symbol_Lists.Symbol_List);

      --------------------
      -- Create_Context --
      --------------------

      procedure Create_Context
        (Unit_Name : Program.Symbol_Lists.Symbol_List)
      is
         Parent : constant Program.Symbol_Lists.Symbol_List :=
           Lists.Prefix (Unit_Name);
      begin
         if Unit.Is_Library_Unit_Declaration then
            Context.Restore_Snapshot (Library.Public_View (Parent));
         else
            null;  --  FIXME
         end if;
      end Create_Context;

      Global : aliased Global_Snapshots.Snapshot_Registry
        (Lists'Unchecked_Access, Library'Unchecked_Access);
      Unit_Name : Program.Symbol_Lists.Symbol_List;
      Element : constant Program.Elements.Element_Access :=
        Unit.Unit_Declaration;
      Visitor   : Visitors.Visitor (Context, Element, Setter);
      EL        : Environment_Level.Visitor
        (Unit_Name_Resolver, Setter);
   begin
      EL.Visit (Element);

      for Clause in Unit.Context_Clause_Elements.Each_Element loop
         EL.Visit (Clause.Element);
      end loop;

      Program.Node_Symbols.Unit_Full_Name (Lists, Unit, Unit_Name);
      Create_Context (Unit_Name);
      Global.Parent := Lists.Prefix (Unit_Name);
      Visitor.Snapshot_Registry := Global'Unchecked_Access;
      Element.Visit (Visitor);
   end Resolve_Names;

   --------------
   -- Visitors --
   --------------

   package body Visitors is

      procedure Visit_Each_Child
        (Self    : in out Visitor;
         Element : access Program.Elements.Element'Class);
      pragma Unreferenced (Visit_Each_Child);

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
         Symbol : constant Program.Symbols.Symbol :=
           Program.Node_Symbols.Get_Symbol (Name);
      begin
         Self.Env.Create_Package
           (Symbol => Symbol,
            Name   => Name);

         for Cursor in Element.Visible_Declarations.Each_Element loop
            Cursor.Element.Visit (Self);
         end loop;

         Self.Snapshot_Registry.Put_Public_View
           (Symbol, Self.Env.Create_Snapshot);

         if not Element.Private_Declarations.Is_Empty then
            raise Program_Error;
         end if;

         Self.Env.Leave_Declarative_Region;
      end Package_Declaration;

      -------------------------
      -- Subtype_Declaration --
      -------------------------

      overriding procedure Subtype_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Subtype_Declarations
           .Subtype_Declaration_Access)
      is
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
            Type_View);

         Self.Env.Create_Subtype
           (Symbol         => Program.Node_Symbols.Get_Symbol (Subtype_Name),
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

end Program.Resolvers;
