------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;
with Gela.Simple_Compilation_Units;
with Gela.Nodes.Visiters;
with Gela.Stores.Compilations;
with Gela.Stores.Compilation_Unit_Bodies;
with Gela.Stores.Compilation_Unit_Declarations;
with Gela.Stores.Defining_Expanded_Unit_Names;
with Gela.Stores.Defining_Identifiers;
with Gela.Stores.Defining_Operator_Symbols;
with Gela.Stores.Function_Bodies;
with Gela.Stores.Function_Declarations;
with Gela.Stores.Function_Instantiations;
with Gela.Stores.Generic_Function_Declarations;
with Gela.Stores.Generic_Function_Renamings;
with Gela.Stores.Generic_Package_Declarations;
with Gela.Stores.Generic_Package_Renamings;
with Gela.Stores.Generic_Procedure_Declarations;
with Gela.Stores.Generic_Procedure_Renamings;
with Gela.Stores.Identifiers;
with Gela.Stores.Package_Instantiations;
with Gela.Stores.Package_Bodies;
with Gela.Stores.Package_Declarations;
with Gela.Stores.Package_Renaming_Declarations;
with Gela.Stores.Procedure_Bodies;
with Gela.Stores.Procedure_Declarations;
with Gela.Stores.Procedure_Instantiations;
with Gela.Stores.Protected_Bodies;
with Gela.Stores.Selected_Identifiers;
with Gela.Stores.Subunits;
with Gela.Stores.Task_Bodies;

package body Gela.Simple_Contexts.Loaders is

   package Visiters is
      type Visiter (Context : Context_Access) is new
        Gela.Nodes.Visiters.Visiter with
      record
         Comp       : Gela.Mutables.Mutable_Compilation_Access;
         Full_Name  : Gela.Types.Symbol;
         Kind       : Gela.Types.Unit_Kinds;
         Origin     : Gela.Types.Unit_Origins;
      end record;

      overriding procedure Compilation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Compilation);

      overriding procedure Compilation_Unit_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Compilation_Unit_Body);

      overriding procedure Compilation_Unit_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Compilation_Unit_Declaration);

      overriding procedure Defining_Expanded_Unit_Name
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Defining_Expanded_Unit_Name);

      overriding procedure Defining_Identifier
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Defining_Identifier);

      overriding procedure Defining_Operator_Symbol
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Defining_Operator_Symbol);

      overriding procedure Function_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Function_Body);

      overriding procedure Function_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Function_Declaration);

      overriding procedure Function_Instantiation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Function_Instantiation);

      overriding procedure Generic_Function_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Function_Declaration);

      overriding procedure Generic_Function_Renaming
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Function_Renaming);

      overriding procedure Generic_Package_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Package_Declaration);

      overriding procedure Generic_Package_Renaming
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Package_Renaming);

      overriding procedure Generic_Procedure_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Procedure_Declaration);

      overriding procedure Generic_Procedure_Renaming
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Procedure_Renaming);

      overriding procedure Identifier
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Identifier);

      overriding procedure Package_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Body);

      overriding procedure Package_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Declaration);

      overriding procedure Package_Instantiation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Instantiation);

      overriding procedure Package_Renaming_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Renaming_Declaration);

      overriding procedure Procedure_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Procedure_Body);

      overriding procedure Procedure_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Procedure_Declaration);

      overriding procedure Procedure_Instantiation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Procedure_Instantiation);

      overriding procedure Protected_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Protected_Body);

      overriding procedure Selected_Identifier
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Selected_Identifier);

      overriding procedure Subunit
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Subunit);

      overriding procedure Task_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Task_Body);

   end Visiters;

   package body Visiters is
      package S renames Gela.Stores;

      overriding procedure Compilation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Compilation)
      is
         This : constant S.Compilations.Object_Access :=
             S.Compilations.Object_Access (Node.its);
         Units : Gela.Nodes.Compilation_Unit_Sequence :=
           This.Units (Node.Payload);
      begin
         Units.its.Visit_Each (Units.Payload, Self);
      end Compilation;

      overriding procedure Compilation_Unit_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Compilation_Unit_Body)
      is
         use type Gela.Types.Payload;

         This : constant S.Compilation_Unit_Bodies.Object_Access :=
             S.Compilation_Unit_Bodies.Object_Access (Node.its);
         Unit             : Gela.Types.Compilation_Unit;
         Unit_Declaration : Gela.Nodes.Library_Unit_Body :=
           This.Unit_Declaration (Node.Payload);
         Payload          : Gela.Types.Payload;
         Ok               : Boolean;
         Class            : Gela.Types.Unit_Classes;
      begin
         Unit_Declaration.its.Visit (Unit_Declaration.Payload, Self);

         Payload := Gela.Types.Payload (Self.Full_Name) * 2;

         Self.Context.Loader.Read_Unit_Declaration (Self.Full_Name, Unit, Ok);

         if not Ok then
            if Self.Kind not in
              Gela.Types.A_Procedure_Body .. Gela.Types.A_Function_Body
            then
               Self.Context.On_Error.No_Compilation_Unit_Declaration
                 (Self.Context.Symbols.Value (Self.Full_Name));
               --  Can't proceed without unit declaration
               return;
            end if;
            Class := Gela.Types.A_Public_Declaration_And_Body;
         else
            Class := Unit.Object.Unit_Class (Unit.Payload);

            case Class is
               when Gela.Types.A_Public_Declaration =>
                  Class := Gela.Types.A_Public_Body;
               when Gela.Types.A_Private_Declaration =>
                  Class := Gela.Types.A_Private_Body;
               when others =>
                  raise Constraint_Error;
            end case;
         end if;

         Unit := Gela.Simple_Compilation_Units.Create
             (Compilation => Self.Comp,
              Payload     => Payload,
              Kind        => Self.Kind,
              Unit_Class  => Class,
              Origin      => Self.Origin,
              Full_Name   => Self.Full_Name);

         Self.Context.Specs.Map.Insert (Self.Full_Name, Unit);
         Self.Context.Units.Insert (Payload, Unit.Object);
      end Compilation_Unit_Body;

      overriding procedure Compilation_Unit_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Compilation_Unit_Declaration)
      is
         use type Gela.Types.Payload;
         use type Gela.Nodes.Token_Access;

         This : constant S. Compilation_Unit_Declarations.Object_Access :=
             S.Compilation_Unit_Declarations.Object_Access (Node.its);
         Unit_Declaration : Gela.Nodes.Library_Unit_Declaration :=
           This.Unit_Declaration (Node.Payload);
         Unit             : Gela.Types.Compilation_Unit;
         Payload          : Gela.Types.Payload;
         Unit_Class       : Gela.Types.Unit_Classes;
         Is_Private       : constant Boolean :=
           This.Private_Token (Node.Payload).its /= null;
      begin
         Unit_Declaration.its.Visit (Unit_Declaration.Payload, Self);

         Payload := Gela.Types.Payload (Self.Full_Name) * 2 + 1;

         if Is_Private then
            Unit_Class := Gela.Types.A_Private_Declaration;
         else
            Unit_Class := Gela.Types.A_Public_Declaration;
         end if;

         Unit := Gela.Simple_Compilation_Units.Create
             (Compilation => Self.Comp,
              Payload     => Payload,
              Kind        => Self.Kind,
              Unit_Class  => Unit_Class,
              Origin      => Self.Origin,
              Full_Name   => Self.Full_Name);

         Self.Context.Specs.Map.Insert (Self.Full_Name, Unit);
         Self.Context.Units.Insert (Payload, Unit.Object);
      end Compilation_Unit_Declaration;

      overriding procedure Defining_Expanded_Unit_Name
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Defining_Expanded_Unit_Name)
      is
         This : constant S.Defining_Expanded_Unit_Names.Object_Access :=
             S.Defining_Expanded_Unit_Names.Object_Access (Node.its);
         Defining_Prefix : Gela.Nodes.Program_Unit_Name :=
           This.Defining_Prefix (Node.Payload);
         Defining_Selector : Gela.Nodes.Defining_Identifier :=
           This.Defining_Selector (Node.Payload);
         Prefix_Full_Name : Gela.Types.Symbol;
      begin
         Defining_Prefix.its.Visit (Defining_Prefix.Payload, Self);
         Prefix_Full_Name := Self.Full_Name;
         Defining_Selector.its.Visit (Defining_Selector.Payload, Self);
         Self.Context.Symbols.Join
           (Prefix  => Prefix_Full_Name,
            Selector => Self.Full_Name,
            Result   => Self.Full_Name);
      end Defining_Expanded_Unit_Name;

      overriding procedure Defining_Identifier
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Defining_Identifier)
      is
         This : constant S.Defining_Identifiers.Object_Access :=
             S.Defining_Identifiers.Object_Access (Node.its);
         Identifier_Token : Gela.Nodes.Token :=
           This.Identifier_Token (Node.Payload);
      begin
         Self.Full_Name :=
           Identifier_Token.its.Symbol (Identifier_Token.Payload);
      end Defining_Identifier;

      overriding procedure Defining_Operator_Symbol
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Defining_Operator_Symbol)
      is
         This : constant S.Defining_Operator_Symbols.Object_Access :=
             S.Defining_Operator_Symbols.Object_Access (Node.its);
         Operator_Symbol_Token : Gela.Nodes.Token :=
           This.Operator_Symbol_Token (Node.Payload);
      begin
         Self.Full_Name :=
           Operator_Symbol_Token.its.Symbol (Operator_Symbol_Token.Payload);
      end Defining_Operator_Symbol;

      overriding procedure Function_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Function_Body)
      is
         This : constant S.Function_Bodies.Object_Access :=
             S.Function_Bodies.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Designator :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Function_Body;
      end Function_Body;

      overriding procedure Function_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Function_Declaration)
      is
         This : constant S.Function_Declarations.Object_Access :=
             S.Function_Declarations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Designator :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Function;
      end Function_Declaration;

      overriding procedure Function_Instantiation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Function_Instantiation)
      is
         This : constant S.Function_Instantiations.Object_Access :=
             S.Function_Instantiations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Designator :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Function_Instance;
      end Function_Instantiation;

      overriding procedure Generic_Function_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Function_Declaration)
      is
         This : constant S.Generic_Function_Declarations.Object_Access :=
             S.Generic_Function_Declarations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Designator :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Generic_Function;
      end Generic_Function_Declaration;

      overriding procedure Generic_Function_Renaming
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Function_Renaming)
      is
         This : constant S.Generic_Function_Renamings.Object_Access :=
             S.Generic_Function_Renamings.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Generic_Function_Renaming;
      end Generic_Function_Renaming;

      overriding procedure Generic_Package_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Package_Declaration)
      is
         This : constant S.Generic_Package_Declarations.Object_Access :=
             S.Generic_Package_Declarations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Generic_Package;
      end Generic_Package_Declaration;

      overriding procedure Generic_Package_Renaming
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Package_Renaming)
      is
         This : constant S.Generic_Package_Renamings.Object_Access :=
             S.Generic_Package_Renamings.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Generic_Package_Renaming;
      end Generic_Package_Renaming;

      overriding procedure Generic_Procedure_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Procedure_Declaration)
      is
         This : constant S.Generic_Procedure_Declarations.Object_Access :=
             S.Generic_Procedure_Declarations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Generic_Procedure;
      end Generic_Procedure_Declaration;

      overriding procedure Generic_Procedure_Renaming
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Generic_Procedure_Renaming)
      is
         This : constant S.Generic_Procedure_Renamings.Object_Access :=
             S.Generic_Procedure_Renamings.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Generic_Procedure_Renaming;
      end Generic_Procedure_Renaming;

      overriding procedure Identifier
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Identifier)
      is
         This : constant S.Identifiers.Object_Access :=
             S.Identifiers.Object_Access (Node.its);
         Identifier_Token : Gela.Nodes.Token :=
           This.Identifier_Token (Node.Payload);
      begin
         Self.Full_Name :=
           Identifier_Token.its.Symbol (Identifier_Token.Payload);
      end Identifier;

      overriding procedure Package_Instantiation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Instantiation)
      is
         This : constant S.Package_Instantiations.Object_Access :=
             S.Package_Instantiations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Package_Instance;
      end Package_Instantiation;

      overriding procedure Package_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Body)
      is
         This : constant S.Package_Bodies.Object_Access :=
             S.Package_Bodies.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Package_Body;
      end Package_Body;

      overriding procedure Package_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Declaration)
      is
         This : constant S.Package_Declarations.Object_Access :=
             S.Package_Declarations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Package;
      end Package_Declaration;

      overriding procedure Package_Renaming_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Package_Renaming_Declaration)
      is
         This : constant S.Package_Renaming_Declarations.Object_Access :=
             S.Package_Renaming_Declarations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Package_Renaming;
      end Package_Renaming_Declaration;

      overriding procedure Procedure_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Procedure_Body)
      is
         This : constant S.Procedure_Bodies.Object_Access :=
             S.Procedure_Bodies.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Procedure_Body;
      end Procedure_Body;

      overriding procedure Procedure_Declaration
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Procedure_Declaration)
      is
         This : constant S.Procedure_Declarations.Object_Access :=
             S.Procedure_Declarations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Procedure;
      end Procedure_Declaration;

      overriding procedure Procedure_Instantiation
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Procedure_Instantiation)
      is
         This : constant S.Procedure_Instantiations.Object_Access :=
             S.Procedure_Instantiations.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Program_Unit_Name :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Procedure_Instance;
      end Procedure_Instantiation;

      overriding procedure Protected_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Protected_Body)
      is
         This : constant S.Protected_Bodies.Object_Access :=
             S.Protected_Bodies.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Identifier :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Protected_Body_Subunit;
      end Protected_Body;

      overriding procedure Selected_Identifier
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Selected_Identifier)
      is
         This : constant S.Selected_Identifiers.Object_Access :=
             S.Selected_Identifiers.Object_Access (Node.its);
         Prefix : Gela.Nodes.Program_Unit_Name :=
           This.Prefix (Node.Payload);
         Selector : Gela.Nodes.Identifier :=
           This.Selector (Node.Payload);
         Prefix_Full_Name : Gela.Types.Symbol;
      begin
         Prefix.its.Visit (Prefix.Payload, Self);
         Prefix_Full_Name := Self.Full_Name;
         Selector.its.Visit (Selector.Payload, Self);

         Self.Context.Symbols.Join
           (Prefix   => Prefix_Full_Name,
            Selector => Self.Full_Name,
            Result   => Self.Full_Name);
      end Selected_Identifier;

      overriding procedure Subunit
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Subunit)
      is
         use type Gela.Types.Payload;

         This : constant S.Subunits.Object_Access :=
             S.Subunits.Object_Access (Node.its);
         Parent_Unit_Name : Gela.Nodes.Program_Unit_Name :=
           This.Parent_Unit_Name (Node.Payload);
         Unit_Declaration : Gela.Nodes.Proper_Body :=
           This.Unit_Declaration (Node.Payload);
         Ok               : Boolean;
         Unit : Gela.Types.Compilation_Unit;
         Payload : Gela.Types.Payload;
         Parent_Full_Name : Gela.Types.Symbol;
      begin
         Parent_Unit_Name.its.Visit (Parent_Unit_Name.Payload, Self);
         Parent_Full_Name := Self.Full_Name;
         Unit_Declaration.its.Visit (Unit_Declaration.Payload, Self);

         Self.Context.Symbols.Join
           (Prefix   => Parent_Full_Name,
            Selector => Self.Full_Name,
            Result   => Self.Full_Name);

         Payload := Gela.Types.Payload (Self.Full_Name) * 2;

         Self.Context.Loader.Read_Unit_Body (Parent_Full_Name, Unit, Ok);

         if not Ok then
            Self.Context.On_Error.No_Compilation_Unit_Body
              (Self.Context.Symbols.Value (Parent_Full_Name));
            --  Can't proceed without unit declaration
            return;
         end if;

         Unit := Gela.Simple_Compilation_Units.Create
             (Compilation => Self.Comp,
              Payload     => Payload,
              Kind        => Self.Kind,
              Unit_Class  => Gela.Types.A_Separate_Body,
              Origin      => Self.Origin,
              Full_Name   => Self.Full_Name);

         Self.Context.Specs.Map.Insert (Self.Full_Name, Unit);
         Self.Context.Units.Insert (Payload, Unit.Object);

         case Self.Kind is
            when Gela.Types.A_Procedure_Body =>
               Self.Kind := Gela.Types.A_Procedure_Body_Subunit;
            when Gela.Types.A_Function_Body_Subunit =>
               Self.Kind := Gela.Types.A_Function_Body_Subunit;
            when Gela.Types.A_Package_Body_Subunit =>
               Self.Kind := Gela.Types.A_Package_Body_Subunit;
            when others =>
               null;
         end case;
      end Subunit;

      overriding procedure Task_Body
        (Self    : in out Visiter;
         Node    : Gela.Nodes.Task_Body)
      is
         This : constant S.Task_Bodies.Object_Access :=
             S.Task_Bodies.Object_Access (Node.its);
         Names : Gela.Nodes.Defining_Identifier :=
           This.Names (Node.Payload);
      begin
         Names.its.Visit (Names.Payload, Self);
         Self.Kind := Gela.Types.A_Task_Body_Subunit;
      end Task_Body;

   end Visiters;

   ------------------------------
   -- Read_File_And_Supporters --
   ------------------------------

   procedure Read_File_And_Supporters
     (Self : in out Loader;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String)
   is
      Comp  : Gela.Mutables.Mutable_Compilation_Access;
      Vis   : aliased Visiters.Visiter (Self.Context);
   begin
      Comp := Gela.Mutables.Compilations.Create
        (Name    => Name,
         Context => Gela.Types.Context_Access (Self.Context),
         Source  => Text,
         Errors  => Self.Context.Errors,
         Symbols => Self.Context.Symbols'Access,
         Grammar => Self.Context.Grammar,
         Table   => Self.Context.Table);

      Comp.Start;

      if Comp.Root.its = null then
         Self.Context.On_Error.Syntax_Error (Name);
      end if;

      if Self.Context.Finder.Is_Predefined (Name) then
         Vis.Origin := Gela.Types.A_Predefined_Unit;
      else
         Vis.Origin := Gela.Types.An_Application_Unit;
      end if;

      Vis.Comp := Comp;

      Gela.Nodes.Visitable_Node_Access (Comp.Root.its).Visit
        (Payload => Comp.Root.Payload,
         Visiter => Vis);
   end Read_File_And_Supporters;

   --------------------
   -- Read_Unit_Body --
   --------------------

   not overriding procedure Read_Unit_Body
     (Self      : in out Loader;
      Full_Name : Gela.Types.Symbol;
      Result    : out Gela.Types.Compilation_Unit;
      Found     : out Boolean)
   is
      use type Gela.Types.Compilation_Unit_Access;
      File_Name : League.Strings.Universal_String;
      Name      : League.Strings.Universal_String;
      Text      : League.Strings.Universal_String;
   begin
      Result := Self.Context.Compilation_Unit_Body (Full_Name);

      if Result.Object /= null then
         Found := True;
         return;
      end if;

      File_Name := Self.Context.Symbols.Value (Full_Name);
      File_Name := Self.Context.Schema.Body_Name (File_Name);

      Self.Context.Finder.Lookup_File (File_Name, Found, Name, Text);

      if Found then
         Self.Read_File_And_Supporters (Name, Text);
         Result := Self.Context.Compilation_Unit_Body (Full_Name);
         Found := Result.Object /= null;
      end if;
   end Read_Unit_Body;

   ---------------------------
   -- Read_Unit_Declaration --
   ---------------------------

   not overriding procedure Read_Unit_Declaration
     (Self      : in out Loader;
      Full_Name : Gela.Types.Symbol;
      Result    : out Gela.Types.Compilation_Unit;
      Found     : out Boolean)
   is
      use type Gela.Types.Compilation_Unit_Access;
      File_Name : League.Strings.Universal_String;
      Name      : League.Strings.Universal_String;
      Text      : League.Strings.Universal_String;
   begin
      Result := Self.Context.Library_Unit_Declaration (Full_Name);

      if Result.Object /= null then
         Found := True;
         return;
      end if;

      File_Name := Self.Context.Symbols.Value (Full_Name);
      File_Name := Self.Context.Schema.Declaration_Name (File_Name);

      Self.Context.Finder.Lookup_File (File_Name, Found, Name, Text);

      if Found then
         Self.Read_File_And_Supporters (Name, Text);
         Result := Self.Context.Library_Unit_Declaration (Full_Name);
         Found := Result.Object /= null;
      end if;
   end Read_Unit_Declaration;

   ----------------------------------
   -- Try_Read_File_And_Supporters --
   ----------------------------------

   procedure Try_Read_File_And_Supporters
     (Self      : in out Loader;
      File_Name : League.Strings.Universal_String)
   is
      Found : Boolean;
      Name  : League.Strings.Universal_String;
      Text  : League.Strings.Universal_String;
   begin
      Self.Context.Finder.Lookup_File (File_Name, Found, Name, Text);

      if not Found then
         Self.Context.On_Error.File_Not_Found (File_Name);
      else
         Self.Read_File_And_Supporters (Name, Text);
      end if;
   end Try_Read_File_And_Supporters;

end Gela.Simple_Contexts.Loaders;
