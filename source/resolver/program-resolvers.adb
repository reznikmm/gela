--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Complete_Contexts.Assignment_Statements;
with Program.Complete_Contexts.Call_Statements;
with Program.Element_Filters;
with Program.Element_Vectors;
with Program.Elements.Assignment_Statements;
with Program.Elements.Call_Statements;
with Program.Elements.Component_Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Names;
with Program.Elements.Discriminant_Specifications;
with Program.Elements.Enumeration_Literal_Specifications;
with Program.Elements.Function_Declarations;
with Program.Elements.Identifiers;
with Program.Elements.Object_Declarations;
with Program.Elements.Package_Declarations;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Procedure_Body_Declarations;
with Program.Elements.Procedure_Declarations;
with Program.Elements.Record_Definitions;
with Program.Elements.Record_Types;
with Program.Elements.Use_Clauses;
with Program.Elements.With_Clauses;
with Program.Elements.Variant_Parts;
with Program.Elements.Variants;
with Program.Elements;
with Program.Interpretations;
with Program.Lexical_Elements;
with Program.Node_Symbols;
with Program.Resolvers.Basic;
with Program.Resolvers.Name_In_Region;
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
         Clause : Program.Element_Vectors.Element_Vector_Access;
         Setter : not null
           Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
      is new Program.Resolvers.Basic.Visitor (Env, Setter) with record
--    is new Program.Safe_Element_Visitors.Safe_Element_Visitor with record
         Snapshot_Registry : Snapshot_Registry_Access;
         Discriminant      : Program.Visibility.View;
      end record;

      overriding procedure Assignment_Statement
        (Self    : in out Visitor;
         Element : not null Program.Elements.Assignment_Statements
           .Assignment_Statement_Access);

      overriding procedure Call_Statement
        (Self    : in out Visitor;
         Element : not null Program.Elements.Call_Statements
           .Call_Statement_Access);

      overriding procedure Component_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Component_Declarations
           .Component_Declaration_Access);

      overriding procedure Discriminant_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discriminant_Specifications
           .Discriminant_Specification_Access);

      overriding procedure Enumeration_Literal_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification_Access);

      overriding procedure Function_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Function_Declarations
           .Function_Declaration_Access);

      overriding procedure Object_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Object_Declarations
           .Object_Declaration_Access);

      overriding procedure Package_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

      overriding procedure Parameter_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Parameter_Specifications
           .Parameter_Specification_Access);

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access);

      overriding procedure Procedure_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Declarations
           .Procedure_Declaration_Access);

      overriding procedure Record_Definition
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Definitions
           .Record_Definition_Access);

      overriding procedure Record_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Types.Record_Type_Access);

      overriding procedure Variant
        (Self    : in out Visitor;
         Element : not null Program.Elements.Variants.Variant_Access);

      overriding procedure Variant_Part
        (Self    : in out Visitor;
         Element : not null Program.Elements.Variant_Parts
           .Variant_Part_Access);

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
         Name  : Program.Symbols.Symbol)
      is
         Next : Program.Symbol_Lists.Symbol_List;
      begin
         Self.Lists.Find_Or_Create (Self.Parent, Name, Next);
         Self.Parent := Next;
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
      Visitor   : Visitors.Visitor
        (Context, Element, Unit.Context_Clause_Elements, Setter);
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

      procedure Append_Unit_Use_Clauses
        (Self    : in out Visitor'Class;
         Element : Program.Elements.Element_Access);

      -----------------------------
      -- Append_Unit_Use_Clauses --
      -----------------------------

      procedure Append_Unit_Use_Clauses
        (Self : in out Visitor'Class;
         Element : Program.Elements.Element_Access)
      is
         use type Program.Elements.Element_Access;
         Clause : Program.Elements.Use_Clauses.Use_Clause_Access;
      begin
         if Self.Unit /= Element then  --  Return if nested element
            return;
         end if;

         for Item in Self.Clause.Each_Element
           (Program.Element_Filters.Is_Use_Clause'Access)
         loop
            Clause := Item.Element.To_Use_Clause;

            for Name in Clause.Clause_Names.Each_Element loop
               declare
                  View : constant Program.Visibility.View :=
                    Self.Env.Get_Name_View (Name.Element);
               begin
                  if Clause.Has_Type then
                     raise Program_Error;
                  else
                     Self.Env.Add_Use_Package (View);
                  end if;
               end;
            end loop;
         end loop;
      end Append_Unit_Use_Clauses;

      --------------------------
      -- Assignment_Statement --
      --------------------------

      overriding procedure Assignment_Statement
        (Self    : in out Visitor;
         Element : not null Program.Elements.Assignment_Statements
           .Assignment_Statement_Access)
      is
         Sets : aliased Program.Interpretations.Context (Self.Env);
      begin
         Program.Complete_Contexts.Assignment_Statements.Assignment_Statement
           (Sets'Unchecked_Access, Self.Setter, Element);
      end Assignment_Statement;

      --------------------
      -- Call_Statement --
      --------------------

      overriding procedure Call_Statement
        (Self    : in out Visitor;
         Element : not null Program.Elements.Call_Statements
           .Call_Statement_Access)
      is
         Sets : aliased Program.Interpretations.Context (Self.Env);
      begin
         Program.Complete_Contexts.Call_Statements.Call_Statement
           (Sets'Unchecked_Access, Self.Setter, Element);
      end Call_Statement;

      ---------------------------
      -- Component_Declaration --
      ---------------------------

      overriding procedure Component_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Component_Declarations
           .Component_Declaration_Access)
      is
         Sets : aliased Program.Interpretations.Context (Self.Env);

         Has_Default : constant Boolean := Element.Default_Expression.Assigned;
         Type_View : Program.Visibility.View;

         Names : constant Program.Elements.Defining_Identifiers
           .Defining_Identifier_Vector_Access := Element.Names;
      begin
         for Name in Names.Each_Element loop
            declare
               Symbol : constant Program.Symbols.Symbol :=
                 Program.Node_Symbols.Get_Symbol (Name.Element);
            begin
               Self.Env.Create_Component
                 (Symbol,
                  Name.Element.To_Defining_Name,
                  Has_Default);

               Program.Type_Resolvers.Resolve_Type_Definition
                 (Element.Object_Subtype.Subtype_Indication,
                  Self.Env,
                  Self.Setter,
                  Sets'Unchecked_Access,
                  Type_View);

               Self.Env.Leave_Declarative_Region;
               Self.Env.Set_Object_Type (Type_View);
            end;
         end loop;
      end Component_Declaration;

      --------------------------------
      -- Discriminant_Specification --
      --------------------------------

      overriding procedure Discriminant_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discriminant_Specifications
           .Discriminant_Specification_Access)
      is
         Sets : aliased Program.Interpretations.Context (Self.Env);

         Has_Default : constant Boolean := Element.Default_Expression.Assigned;
         Type_View : Program.Visibility.View;

         Names : constant Program.Elements.Defining_Identifiers
           .Defining_Identifier_Vector_Access := Element.Names;
      begin
         for Name in Names.Each_Element loop
            declare
               Symbol : constant Program.Symbols.Symbol :=
                 Program.Node_Symbols.Get_Symbol (Name.Element);
            begin
               Self.Env.Create_Component
                 (Symbol,
                  Name.Element.To_Defining_Name,
                  Has_Default);

               Program.Type_Resolvers.Resolve_Type_Definition
                 (Element.Object_Subtype.To_Element,
                  Self.Env,
                  Self.Setter,
                  Sets'Unchecked_Access,
                  Type_View);

               Self.Env.Leave_Declarative_Region;
               Self.Env.Set_Object_Type (Type_View);
            end;
         end loop;
      end Discriminant_Specification;

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
               Self.Type_View);
         else
            Self.Env.Create_Enumeration_Literal
              (Symbol,
               Element.Name,
               Self.Type_View);
         end if;
      end Enumeration_Literal_Specification;

      ------------------------
      -- Object_Declaration --
      ------------------------

      overriding procedure Object_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Object_Declarations
           .Object_Declaration_Access)
      is
         Type_View : Program.Visibility.View;

         Sets : aliased Program.Interpretations.Context (Self.Env);

         Names : constant Program.Elements.Defining_Identifiers
           .Defining_Identifier_Vector_Access := Element.Names;
      begin
         for Name in Names.Each_Element loop
            declare
               Symbol : constant Program.Symbols.Symbol :=
                 Program.Node_Symbols.Get_Symbol (Name.Element);
            begin
               Self.Env.Create_Variable
                 (Symbol,
                  Name.Element.To_Defining_Name);

               Program.Type_Resolvers.Resolve_Type_Definition
                 (Element.Object_Subtype.To_Element,
                  Self.Env,
                  Self.Setter,
                  Sets'Unchecked_Access,
                  Type_View);

               if Element.Initialization_Expression.Assigned then
                  Program.Complete_Contexts.Resolve_To_Expected_Type
                    (Element => Program.Elements.Element_Access
                       (Element.Initialization_Expression),
                     Sets    => Sets'Unchecked_Access,
                     Setter  => Self.Setter,
                     Expect  => Program.Visibility.First_Subtype (Type_View));
               end if;

               Self.Env.Leave_Declarative_Region;
               Self.Env.Set_Object_Type (Type_View);
            end;
         end loop;
      end Object_Declaration;

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

         Self.Append_Unit_Use_Clauses (Element.all'Access);

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

      -----------------------------
      -- Parameter_Specification --
      -----------------------------

      overriding procedure Parameter_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Access)
      is
         Sets : aliased Program.Interpretations.Context (Self.Env);

         Modes : constant array (Boolean, Boolean) of
           Program.Visibility.Parameter_Mode :=
             (False =>
                (False => Program.Visibility.In_Mode,
                 True  => Program.Visibility.Out_Mode),
              True  =>
                (False => Program.Visibility.In_Mode,
                 True  => Program.Visibility.In_Out_Mode));

         Mode : constant Program.Visibility.Parameter_Mode :=
           Modes  (Element.Has_In, Element.Has_Out);

         Has_Default : constant Boolean := Element.Default_Expression.Assigned;
         Type_View : Program.Visibility.View;

         Names : constant Program.Elements.Defining_Identifiers
           .Defining_Identifier_Vector_Access := Element.Names;
      begin
         for Name in Names.Each_Element loop
            declare
               Symbol : constant Program.Symbols.Symbol :=
                 Program.Node_Symbols.Get_Symbol (Name.Element);
            begin
               Self.Env.Create_Parameter
                 (Symbol,
                  Name.Element.To_Defining_Name,
                  Mode,
                  Has_Default);

               Program.Type_Resolvers.Resolve_Type_Definition
                 (Element.Parameter_Subtype,
                  Self.Env,
                  Self.Setter,
                  Sets'Unchecked_Access,
                  Type_View);

               Self.Env.Leave_Declarative_Region;
               Self.Env.Set_Object_Type (Type_View);
            end;
         end loop;
      end Parameter_Specification;

      --------------------------------
      -- Procedure_Body_Declaration --
      --------------------------------

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access)
      is
         Name   : constant
           Program.Elements.Defining_Names.Defining_Name_Access :=
             Element.Name;
         Symbol : constant Program.Symbols.Symbol :=
           Program.Node_Symbols.Get_Symbol (Name);
      begin
         Self.Env.Create_Procedure
           (Symbol => Symbol,
            Name   => Name);

         Self.Append_Unit_Use_Clauses (Element.all'Access);

         for Cursor in Element.Parameters.Each_Element loop
            Cursor.Element.Visit (Self);
         end loop;

         for Cursor in Element.Declarations.Each_Element loop
            Cursor.Element.Visit (Self);
         end loop;

         for Cursor in Element.Statements.Each_Element loop
            Cursor.Element.Visit (Self);
         end loop;

         for Cursor in Element.Exception_Handlers.Each_Element loop
            Cursor.Element.Visit (Self);
         end loop;

         Self.Env.Leave_Declarative_Region;
      end Procedure_Body_Declaration;

      overriding procedure Function_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Function_Declarations
           .Function_Declaration_Access)
      is
         Name   : constant
           Program.Elements.Defining_Names.Defining_Name_Access :=
             Element.Name;
         Symbol : constant Program.Symbols.Symbol :=
           Program.Node_Symbols.Get_Symbol (Name);
         Type_View : Program.Visibility.View;
         Sets : aliased Program.Interpretations.Context (Self.Env);
      begin
         Self.Env.Create_Function
           (Symbol => Symbol,
            Name   => Name);

         Self.Append_Unit_Use_Clauses (Element.all'Access);

         for Cursor in Element.Parameters.Each_Element loop
            Cursor.Element.Visit (Self);
         end loop;

         for Aspect in Element.Aspects.Each_Element loop
            Aspect.Element.Visit (Self);
         end loop;

         Program.Type_Resolvers.Resolve_Type_Definition
           (Element.Result_Subtype,
            Self.Env,
            Self.Setter,
            Sets'Unchecked_Access,
            Type_View);

         Self.Env.Set_Result_Type (Type_View);
         Self.Env.Leave_Declarative_Region;
      end Function_Declaration;

      ---------------------------
      -- Procedure_Declaration --
      ---------------------------

      overriding procedure Procedure_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Declarations
           .Procedure_Declaration_Access)
      is
         Name   : constant
           Program.Elements.Defining_Names.Defining_Name_Access :=
             Element.Name;
         Symbol : constant Program.Symbols.Symbol :=
           Program.Node_Symbols.Get_Symbol (Name);
      begin
         Self.Env.Create_Procedure
           (Symbol => Symbol,
            Name   => Name);

         Self.Append_Unit_Use_Clauses (Element.all'Access);

         for Cursor in Element.Parameters.Each_Element loop
            Cursor.Element.Visit (Self);
         end loop;

         for Aspect in Element.Aspects.Each_Element loop
            Aspect.Element.Visit (Self);
         end loop;

         Self.Env.Leave_Declarative_Region;
      end Procedure_Declaration;

      overriding procedure Record_Definition
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Definitions
           .Record_Definition_Access) is
      begin
         Self.Env.Create_Record_Type
           (Symbol => Program.Node_Symbols.Get_Symbol (Self.Type_Name),
            Name   => Self.Type_Name);
         Self.Type_View := Self.Env.Latest_View;

         if Self.Discriminants.Is_Known_Discriminant_Part then
            declare
               List : constant Program.Elements.Discriminant_Specifications
                 .Discriminant_Specification_Vector_Access :=
                   Self.Discriminants.To_Known_Discriminant_Part.Discriminants;
            begin
               for J in List.Each_Element loop
                  J.Element.Visit (Self);
               end loop;
            end;
         end if;

         for J in Element.Components.Each_Element loop
            J.Element.Visit (Self);
         end loop;

         Self.Env.Leave_Declarative_Region;
      end Record_Definition;

      overriding procedure Record_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Types.Record_Type_Access)
      is
      begin
         Self.Visit_Each_Child (Element);
      end Record_Type;

      overriding procedure Variant
        (Self    : in out Visitor;
         Element : not null Program.Elements.Variants.Variant_Access)
      is
         Sets : aliased Program.Interpretations.Context (Self.Env);
      begin
         for J in Element.Choices.Each_Element loop
            Program.Complete_Contexts.Resolve_To_Expected_Type
              (Element => J.Element,
               Sets    => Sets'Unchecked_Access,
               Setter  => Self.Setter,
               Expect  => Program.Visibility.Subtype_Mark (Self.Discriminant));
         end loop;

         Self.Visit_Each_Child (Element);
      end Variant;

      overriding procedure Variant_Part
        (Self    : in out Visitor;
         Element : not null Program.Elements.Variant_Parts
           .Variant_Part_Access) is
      begin
         Program.Resolvers.Name_In_Region.Resolve_Name
           (Region => Self.Type_View,
            Name   => Element.Discriminant.To_Expression,
            Setter => Self.Setter);

         Self.Discriminant := Self.Env.Get_Name_View
           (Element.Discriminant.To_Element);

         Self.Visit_Each_Child (Element);
      end Variant_Part;

   end Visitors;

end Program.Resolvers;
