--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with System.Storage_Pools.Subpools;

with Program.Directory_Unit_Schemas;
with Program.GNAT_Unit_Naming;
with Program.Parsers;
with Program.Plain_Compilations;
with Program.Resolve_Standard;
with Program.Storage_Pools;
with Program.Unit_Dependencies;

package body Program.Plain_Contexts is

   type Plain_Compilation_Access is
     access all Program.Plain_Compilations.Compilation;

   type Unit_Naming_Schema_Access is
     access all Program.Unit_Naming.Unit_Naming_Schema'Class;

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
         Lists  : in out Program.Symbol_Lists.Symbol_List_Table'Class;
         Report : in out Unit_Dependencies.Unit_Dependency_Listener'Class));

   overriding procedure Required_Declaration
     (Self   : in out Dependency;
      Name   : Program.Symbol_Lists.Symbol_List;
      If_Any : Boolean := False);
   --  Library unit declaration is required (if any when If_Any).

   overriding procedure Required_Body
     (Self : in out Dependency;
      Name : Program.Symbol_Lists.Symbol_List);
   --  Library unit body or subunit is required

   overriding procedure Required_Unit
     (Self       : in out Dependency;
      Name       : Program.Symbol_Lists.Symbol_List;
      Is_Limited : Boolean);

   procedure Parse_File
     (Self      : aliased in out Context'Class;
      Text_Name : Text;
      Standard  : Boolean;
      Units     : out Program.Parsers.Unit_Vectors.Vector;
      Pragmas   : out Program.Parsers.Element_Vectors.Vector);

   --------------------------
   -- Add_Search_Directory --
   --------------------------

   procedure Add_Search_Directory
     (Self : in out Context'Class;
      Path : Program.Text) is
   begin
      Program.Directory_Unit_Schemas.Directory_Unit_Schema'Class
        (Self.Naming.all).Add_Directory (Path);
   end Add_Search_Directory;

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
      procedure Analyze
        (Vector : in out Unit_Vector;
         Symbol : Program.Symbol_Lists.Symbol_List);
      --  Find and analyze all dependencies and then Unit itself, where Unit
      --  is Vector.Map (Symbol).

      procedure Analyze_Unit
        (Unit : Program.Compilation_Units.Compilation_Unit_Access);

      First : Boolean := True;

      -------------
      -- Analyze --
      -------------

      procedure Analyze
        (Vector : in out Unit_Vector;
         Symbol : Program.Symbol_Lists.Symbol_List)
      is
         Deps : Dependency (Self'Unchecked_Access);
         Item : constant Unit_Map_Item := Vector.Map (Symbol);
      begin
         case Item.Status is
            when Parsed =>
               Vector.Map (Symbol).Status := Loading;

               Deps.Find_Dependecies
                 (Item.Unit,
                  Program.Unit_Dependencies.Find_Semantic_Dependencies'Access);
               --  All dependencies are parsed and listed in Deps

               for J of Deps.Declrations loop
                  Analyze (Self.Declarations, J);
               end loop;

               for J of Deps.Bodies loop
                  Analyze (Self.Bodies, J);
               end loop;

               --  All dependencies analysed
               Analyze_Unit (Item.Unit);
               Vector.Map (Symbol).Status := Analysed;

            when Loading =>

               Self.Errors.Circular_Dependency (Item.Unit.Full_Name);

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
         List : constant Program.Symbol_Lists.Symbol_List :=
           Self.Symbols.Lists.Find (Unit.Full_Name);
      begin
         if First then
            Program.Resolve_Standard (Unit, Self.Visible);

            First := False;
         end if;

         if Unit.Is_Library_Unit_Declaration then
            Self.Library_Env.Put_Public_View
              (List, Self.Visible.Create_Snapshot);
         end if;
      end Analyze_Unit;

      Item      : Program.Symbol_Lists.Symbol_List;
      Last_Decl : constant Natural := Self.Declarations.List.Last_Index;
      Last_Body : constant Natural := Self.Bodies.List.Last_Index;
   begin
      for J in 1 .. Last_Decl loop
         Item := Self.Declarations.List (J);
         Analyze (Self.Declarations, Item);
      end loop;

      for J in 1 .. Last_Body loop
         Item := Self.Bodies.List (J);
         Analyze (Self.Bodies, Item);
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
         Lists  : in out Program.Symbol_Lists.Symbol_List_Table'Class;
         Report : in out Unit_Dependencies.Unit_Dependency_Listener'Class)) is
   begin
      Check (Unit, Self.Context.Symbols.Lists, Self);
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

   procedure Initialize
     (Self   : in out Context'Class;
      Errors : Program.Error_Listeners.Error_Listener_Access)
   is
      GNAT : constant Unit_Naming_Schema_Access := new
        Program.GNAT_Unit_Naming.GNAT_Unit_Naming;
      Dir  : constant Unit_Naming_Schema_Access := new
        Program.Directory_Unit_Schemas.Directory_Unit_Schema
          (GNAT.all'Access);
   begin
      Self.Naming := Dir.all'Access;
      Self.Errors := Errors;
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
      if Self.Declarations.List.Is_Empty then
         return null;
      else
         return Self.Declarations'Unchecked_Access;
      end if;
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

      Self.Compilations.Append
        (Program.Compilations.Compilation_Access (Compilation));
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

   begin
      Self.Parse_File (Text_Name, False, Units, Pragmas);

      for Unit of Units loop
         Self.Append_Unit (Unit);
      end loop;
   end Parse_File;


   -------------------
   -- Required_Body --
   -------------------

   overriding procedure Required_Body
     (Self : in out Dependency;
      Name : Program.Symbol_Lists.Symbol_List)
   is
      Units   : Program.Parsers.Unit_Vectors.Vector;
      Pragmas : Program.Parsers.Element_Vectors.Vector;
      Unit    : Program.Compilation_Units.Compilation_Unit_Access;

      Found   : constant Unit_Maps.Cursor :=
        Self.Context.Bodies.Map.Find (Name);

      Full_Name : constant Program.Text :=
        Self.Context.Symbols.Lists.Symbol_List_Text (Name);

      Text_Name : constant Program.Text :=
        Self.Context.Naming.Body_Text_Name (Full_Name);
   begin
      if Unit_Maps.Has_Element (Found) then
         Self.Bodies.Append (Unit_Maps.Key (Found));

         return;
      elsif Text_Name = "" then
         Self.Context.Errors.No_Body_Text (Full_Name);
         return;  --  TODO: Mark self.context as failed?
      else
         Self.Context.Parse_File
           (Text_Name => Text_Name,
            Standard  => False,
            Units     => Units,
            Pragmas   => Pragmas);
      end if;

      pragma Assert (Units.Last_Index = 1);  --  TODO: Check unit.name = Name?

      if Units.Last_Index = 1 then
         Unit := Units (1);
         pragma Assert (Unit.Is_Library_Unit_Body);
         Self.Context.Append_Unit (Unit);
         Self.Bodies.Append (Self.Context.Bodies.List.Last_Element);
      end if;
   end Required_Body;

   --------------------------
   -- Required_Declaration --
   --------------------------

   overriding procedure Required_Declaration
     (Self   : in out Dependency;
      Name   : Program.Symbol_Lists.Symbol_List;
      If_Any : Boolean := False)
   is
      use type Program.Symbol_Lists.Symbol_List;

      Units   : Program.Parsers.Unit_Vectors.Vector;
      Pragmas : Program.Parsers.Element_Vectors.Vector;
      Unit    : Program.Compilation_Units.Compilation_Unit_Access;

      Is_Standard : constant Boolean :=
        Name = Program.Symbol_Lists.Empty_Symbol_List;

      Found   : constant Unit_Maps.Cursor :=
        Self.Context.Declarations.Map.Find (Name);

      Full_Name : constant Program.Text :=
        Self.Context.Symbols.Lists.Symbol_List_Text (Name);

      function Text_Name return Program.Text;

      function Text_Name return Program.Text is
      begin
         if Is_Standard then
            return Self.Context.Naming.Standard_Text_Name;
         else
            return Self.Context.Naming.Declaration_Text_Name (Full_Name);
         end if;
      end Text_Name;

   begin
      if Unit_Maps.Has_Element (Found) then
         Self.Declrations.Append (Unit_Maps.Key (Found));

         return;
      elsif If_Any and not Is_Standard and Text_Name = "" then
         null;  --  Optional unit not found
      else
         Self.Context.Parse_File
           (Text_Name => Text_Name,
            Standard  => Is_Standard,
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

   -------------------
   -- Required_Unit --
   -------------------

   overriding procedure Required_Unit
     (Self       : in out Dependency;
      Name       : Program.Symbol_Lists.Symbol_List;
      Is_Limited : Boolean)
   is
      Saved_Count : constant Natural := Self.Declrations.Last_Index;
   begin
      pragma Assert (not Is_Limited);
      Self.Required_Declaration (Name, If_Any => True);

      if Saved_Count = Self.Declrations.Last_Index then
         Self.Required_Body (Name);
      end if;
   end Required_Unit;

end Program.Plain_Contexts;
