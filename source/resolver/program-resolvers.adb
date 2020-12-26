--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Visitors;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Names;
with Program.Elements.Identifiers;
with Program.Elements.Package_Declarations;
with Program.Elements.Procedure_Body_Declarations;
with Program.Elements.Use_Clauses;
with Program.Elements.With_Clauses;
with Program.Elements;
with Program.Lexical_Elements;
with Program.Node_Symbols;
with Program.Safe_Element_Visitors;
with Program.Symbols;

package body Program.Resolvers is

   package Visitors is

      type Environment_Level
        (Unit_Name_Resolver : not null
           Program.Simple_Resolvers.Simple_Resolver_Access;
         Setter             : not null
           Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
      is new Program.Safe_Element_Visitors.Safe_Element_Visitor
        with null record;

      overriding procedure Package_Declaration
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access);

      overriding procedure Use_Clause
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.Use_Clauses.Use_Clause_Access);

      overriding procedure With_Clause
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.With_Clauses.With_Clause_Access);

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
     (Unit    : not null Program.Compilation_Units.Compilation_Unit_Access;
      Unit_Name_Resolver : not null
        Program.Simple_Resolvers.Simple_Resolver_Access;
      Lists   : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Context : aliased in out Program.Visibility.Context;
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

      Unit_Name : Program.Symbol_Lists.Symbol_List;
      Element : constant Program.Elements.Element_Access :=
        Unit.Unit_Declaration;
      Visitor   : Visitors.Visitor (Context'Access);
      EL        : Visitors.Environment_Level (Unit_Name_Resolver, Setter);
   begin
      EL.Visit (Element);

      for Clause in Unit.Context_Clause_Elements.Each_Element loop
         EL.Visit (Clause.Element);
      end loop;

      Program.Node_Symbols.Unit_Full_Name (Lists, Unit, Unit_Name);
      Create_Context (Unit_Name);
      Element.Visit (Visitor);
      Library.Put_Public_View (Unit_Name, Context.Create_Snapshot);
   end Resolve_Names;

   --------------
   -- Visitors --
   --------------

   package body Visitors is

      -------------------------
      -- Package_Declaration --
      -------------------------

      overriding procedure Package_Declaration
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access)
      is
      begin
         --  Find parent name and resolve it
         null;
      end Package_Declaration;

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
      end Package_Declaration;

      --------------------------------
      -- Procedure_Body_Declaration --
      --------------------------------

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.Procedure_Body_Declarations
         .Procedure_Body_Declaration_Access) is
      begin
         null;  --  FIXME: Find parent name and resolve it
      end Procedure_Body_Declaration;

      ----------------
      -- Use_Clause --
      ----------------

      overriding procedure Use_Clause
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.Use_Clauses.Use_Clause_Access) is
      begin
         null;  --  FIXME:
      end Use_Clause;

      -----------------
      -- With_Clause --
      -----------------

      overriding procedure With_Clause
        (Self    : in out Environment_Level;
         Element : not null Program.Elements.With_Clauses.With_Clause_Access)
      is
      begin
         pragma Assert (not Element.Has_Limited);

         for Name in Element.Clause_Names.Each_Element loop
            Self.Unit_Name_Resolver.Resolve
              (Name.Element.To_Expression, Self.Setter);
         end loop;
      end With_Clause;

   end Visitors;

end Program.Resolvers;
