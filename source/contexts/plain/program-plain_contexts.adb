--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with System.Storage_Pools.Subpools;

with Program.Parsers;
with Program.Plain_Compilations;
with Program.Resolve_Standard;
with Program.Storage_Pools;
with Program.Unit_Dependencies;

package body Program.Plain_Contexts is

   type Plain_Compilation_Access is
     access all Program.Plain_Compilations.Compilation;

   package Symbol_List_Index_Vectors is new Ada.Containers.Vectors
     (Positive,
      Program.Symbol_Lists.Symbol_List,
      "=" => Program.Symbol_Lists."=");

   type Dependency (Context : access Program.Plain_Contexts.Context'Class) is
     new Program.Unit_Dependencies.Unit_Dependency_Listener with
   record
      Declrations : Symbol_List_Index_Vectors.Vector;
      Bodies      : Symbol_List_Index_Vectors.Vector;
   end record;

   procedure Find_Dependecies
     (Self  : in out Dependency'Class;
      Unit  : Program.Compilation_Units.Compilation_Unit_Access;
      Check : access procedure
        (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
         Report : in out Unit_Dependencies.Unit_Dependency_Listener'Class));

   overriding procedure Required_Declaration
     (Self   : in out Dependency;
      Name   : Program.Text;
      If_Any : Boolean := False);
   --  Library unit declaration is required (if any when If_Any).

   overriding procedure Required_Body
     (Self : in out Dependency;
      Name : Program.Text) is null;
   --  Library unit body or subunit is required

   overriding procedure Required_Unit
     (Self       : in out Dependency;
      Name       : Program.Text;
      Is_Limited : Boolean) is null;

   procedure Parse_File
     (Self      : aliased in out Context'Class;
      Text_Name : Text;
      Standard  : Boolean;
      Units     : out Program.Parsers.Unit_Vectors.Vector;
      Pragmas   : out Program.Parsers.Element_Vectors.Vector);

   -----------------
   -- Append_Unit --
   -----------------

   procedure Append_Unit
     (Self : in out Context'Class;
      Unit : Program.Compilation_Units.Compilation_Unit_Access)
   is
      Name  : constant Program.Text := Unit.Full_Name;
      Index : Program.Symbol_Lists.Symbol_List :=
        Program.Symbol_Lists.Empty_Symbol_List;  --  Standard's index is 0
   begin
      if Name /= "" then
         Self.Symbols.Lists.Find_Or_Create (Name, Index);
      end if;

      if Unit.Is_Library_Unit_Declaration then
         Self.Declarations.Map.Insert (Index, (Parsed, Unit));
         Self.Declarations.List.Append (Index);
      else
         Self.Bodies.Map.Insert (Index, (Parsed, Unit));
         Self.Bodies.List.Append (Index);
      end if;
   end Append_Unit;

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   overriding function Compilation_Unit_Bodies (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      if Self.Bodies.List.Is_Empty then
         return null;
      else
         return Self.Bodies'Unchecked_Access;
      end if;
   end Compilation_Unit_Bodies;

   -----------------------
   -- Complete_Analysis --
   -----------------------

   procedure Complete_Analysis (Self : in out Context'Class) is
      procedure Analyze (Item : in out Unit_Map_Item);
      --  Find and analyze all dependencies and then unit itself

      procedure Analyze_Unit
        (Unit : Program.Compilation_Units.Compilation_Unit_Access);

      First : Boolean := True;

      -------------
      -- Analyze --
      -------------

      procedure Analyze (Item : in out Unit_Map_Item) is
         Deps : Dependency (Self'Unchecked_Access);
      begin
         case Item.Status is
            when Parsed =>
               Item.Status := Loading;

               Deps.Find_Dependecies
                 (Item.Unit,
                  Program.Unit_Dependencies.Find_Semantic_Dependencies'Access);
               --  All dependencies are parsed and listed in Deps

               for J of Deps.Declrations loop
                  Analyze (Self.Declarations.Map (J));
               end loop;

               for J of Deps.Bodies loop
                  Analyze (Self.Bodies.Map (J));
               end loop;

               --  All dependencies analysed
               Analyze_Unit (Item.Unit);
               Item.Status := Analysed;

            when Loading =>
               raise Program_Error with "Circular dependency found";
            when Analysed =>
               null;  --  Nothing to do
         end case;
      end Analyze;

      ------------------
      -- Analyze_Unit --
      ------------------

      procedure Analyze_Unit
        (Unit : Program.Compilation_Units.Compilation_Unit_Access)
      is
      begin
         if First then
            Program.Resolve_Standard (Unit, Self.Visible);

            First := False;
         end if;

         if Unit.Is_Library_Unit_Declaration then
            Self.Library_Env.Put_Public_View
              (Self.Symbols.Lists.Find (Unit.Full_Name),
               Self.Visible.Create_Snapshot);
         end if;
      end Analyze_Unit;

      Item      : Unit_Map_Item;  --  To avoid "attempt to tamper with cursors"
      Last_Decl : constant Natural := Self.Declarations.List.Last_Index;
      Last_Body : constant Natural := Self.Bodies.List.Last_Index;
   begin
      for J in 1 .. Last_Decl loop
         Item := Self.Declarations.Map (Self.Declarations.List (J));
         Analyze (Item);
      end loop;

      for J in 1 .. Last_Body loop
         Item := Self.Bodies.Map (Self.Bodies.List (J));
         Analyze (Item);
      end loop;
   end Complete_Analysis;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Unit_Vector;
      Index : Positive)
        return not null Program.Compilation_Units.Compilation_Unit_Access is
   begin
      return Self.Map (Self.List (Index)).Unit;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Self  : Context'Class;
      Value : Program.Text) return Program.Symbols.Symbol is
   begin
      return Self.Symbols.Find (Value);
   end Find;

   ---------------------------
   -- Find_Or_Create_Symbol --
   ---------------------------

   procedure Find_Or_Create_Symbol
     (Self : in out Context'Class;
      Buffer : not null Program.Source_Buffers.Source_Buffer_Access;
      Span   : Program.Source_Buffers.Span;
      Result : out Program.Symbols.Symbol) is
   begin
      Self.Symbols.Find_Or_Create (Buffer, Span, Result);
   end Find_Or_Create_Symbol;

   ----------------------
   -- Find_Dependecies --
   ----------------------

   procedure Find_Dependecies
     (Self  : in out Dependency'Class;
      Unit  : Program.Compilation_Units.Compilation_Unit_Access;
      Check : access procedure
        (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
         Report : in out Unit_Dependencies.Unit_Dependency_Listener'Class)) is
   begin
      Check (Unit, Self);
   end Find_Dependecies;

   ---------------
   -- Find_Unit --
   ---------------

   overriding function Find_Unit
     (Self : Unit_Vector;
      Name : Text) return Program.Compilation_Units.Compilation_Unit_Access
   is
      Cursor : constant Unit_Maps.Cursor := Self.Find_Unit (Name);
   begin
      if Unit_Maps.Has_Element (Cursor) then
         return Unit_Maps.Element (Cursor).Unit;
      else
         return null;
      end if;
   end Find_Unit;

   ---------------
   -- Find_Unit --
   ---------------

   function Find_Unit
     (Self  : Unit_Vector'Class;
      Name  : Text) return Unit_Maps.Cursor
   is
      use type Program.Symbol_Lists.Symbol_List;

      Result : constant Program.Symbol_Lists.Symbol_List :=
        Self.Context.Symbols.Lists.Find (Name);
   begin
      if Result = Program.Symbol_Lists.Empty_Symbol_List
        and then Name /= ""
      then
         return Unit_Maps.No_Element;
      else
         return Self.Map.Find (Result);
      end if;
   end Find_Unit;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (Self : Unit_Vector) return Positive is
   begin
      return Self.List.Last_Index;
   end Get_Length;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Context'Class) is
   begin
      Self.Symbols.Initialize;
   end Initialize;

   -----------------------
   -- Immediate_Visible --
   -----------------------

   function Immediate_Visible
     (Self : in out Context'Class;
      Unit : Program.Text;
      Name : Program.Text) return Program.Visibility.View_Array
   is

   begin
      Self.Visible.Restore_Snapshot
        (Self.Library_Env.Public_View
           (Self.Symbols.Lists.Find (Unit)));

      return Self.Visible.Immediate_Visible
        (Self.Symbols.Find (Name));
   end Immediate_Visible;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   overriding function Library_Unit_Declarations (Self : Context)
     return Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access is
   begin
      return Self.Declarations'Unchecked_Access;
   end Library_Unit_Declarations;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (Self      : aliased in out Context'Class;
      Text_Name : Text;
      Standard  : Boolean;
      Units     : out Program.Parsers.Unit_Vectors.Vector;
      Pragmas   : out Program.Parsers.Element_Vectors.Vector)
   is

      Pool : Program.Storage_Pools.Storage_Pool renames
        Program.Storage_Pools.Pool;

      Subpool : constant not null System.Storage_Pools.Subpools.Subpool_Handle
        := Pool.Create_Subpool;

      Compilation : constant Plain_Compilation_Access :=
        new Program.Plain_Compilations.Compilation (Subpool);
      --  Plain_Compilation is a controlled type, so don't allocate it in
      --  the (Subpool)

   begin
      Compilation.Initialize (Self'Unchecked_Access);

      Compilation.Parse_File (Text_Name, Units, Pragmas, Standard);
   end Parse_File;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (Self      : aliased in out Context'Class;
      Text_Name : Text)
   is
      Units   : Program.Parsers.Unit_Vectors.Vector;
      Pragmas : Program.Parsers.Element_Vectors.Vector;

      Pool : Program.Storage_Pools.Storage_Pool renames
        Program.Storage_Pools.Pool;

      Subpool : constant not null System.Storage_Pools.Subpools.Subpool_Handle
        := Pool.Create_Subpool;

      Compilation : constant Plain_Compilation_Access :=
        new Program.Plain_Compilations.Compilation (Subpool);
      --  Plain_Compilation is a controlled type, so don't allocate it in
      --  the (Subpool)

   begin
      Self.Parse_File (Text_Name, False, Units, Pragmas);

      for Unit of Units loop
         Self.Append_Unit (Unit);
      end loop;

--        Env.Create_Empty_Context;
--
--        Program.Resolve_Standard (Unit => Units (1), Env => Env);

      Self.Compilations.Append
        (Program.Compilations.Compilation_Access (Compilation));
   end Parse_File;


   --------------------------
   -- Required_Declaration --
   --------------------------

   overriding procedure Required_Declaration
     (Self   : in out Dependency;
      Name   : Program.Text;
      If_Any : Boolean := False)
   is
      Units   : Program.Parsers.Unit_Vectors.Vector;
      Pragmas : Program.Parsers.Element_Vectors.Vector;
      Unit    : Program.Compilation_Units.Compilation_Unit_Access;
      Found   : constant Unit_Maps.Cursor :=
        Self.Context.Declarations.Find_Unit (Name);
   begin
      if Unit_Maps.Has_Element (Found) then
         Self.Declrations.Append (Unit_Maps.Key (Found));

         return;
      elsif Name = "" then
         Self.Context.Parse_File
           (Text_Name => Self.Context.Naming.Standard_Text_Name,
            Standard  => True,
            Units     => Units,
            Pragmas   => Pragmas);
      else
         Self.Context.Parse_File
           (Text_Name => Self.Context.Naming.Declaration_Text_Name (Name),
            Standard  => False,
            Units     => Units,
            Pragmas   => Pragmas);
      end if;

      pragma Assert
        (Units.Last_Index = 1 or
           (Units.Last_Index = 0 and If_Any));

      if Units.Last_Index = 1 then
         Unit := Units (1);
         pragma Assert (Unit.Is_Library_Unit_Declaration);
         Self.Context.Append_Unit (Unit);
         Self.Declrations.Append
           (Self.Context.Declarations.List.Last_Element);
      end if;
   end Required_Declaration;

end Program.Plain_Contexts;
