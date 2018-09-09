with Gela.Array_Type_Views;
with Gela.Compilations;
with Gela.Derived_Type_Views;
with Gela.Element_Factories;
with Gela.Element_Visiters;
with Gela.Elements.Access_To_Object_Definitions;
with Gela.Elements.Basic_Declarative_Items;
with Gela.Elements.Component_Declarations;
with Gela.Elements.Component_Definitions;
with Gela.Elements.Composite_Subtype_Indications;
with Gela.Elements.Constrained_Array_Definitions;
with Gela.Elements.Defining_Character_Literals;
with Gela.Elements.Defining_Enumeration_Names;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Derived_Type_Definitions;
with Gela.Elements.Discrete_Simple_Expression_Ranges;
with Gela.Elements.Discrete_Subtype_Indications;
with Gela.Elements.Discriminant_Specifications;
with Gela.Elements.Enumeration_Literal_Specifications;
with Gela.Elements.Enumeration_Type_Definitions;
with Gela.Elements.Floating_Point_Definitions;
with Gela.Elements.Formal_Discrete_Type_Definitions;
with Gela.Elements.Formal_Object_Declarations;
with Gela.Elements.Formal_Signed_Integer_Type_Definitions;
with Gela.Elements.Formal_Type_Definitions;
with Gela.Elements.Identifiers;
with Gela.Elements.Loop_Parameter_Specifications;
with Gela.Elements.Number_Declarations;
with Gela.Elements.Object_Declarations;
with Gela.Elements.Package_Declarations;
with Gela.Elements.Parameter_Specifications;
with Gela.Elements.Record_Type_Definitions;
with Gela.Elements.Scalar_Subtype_Indications;
with Gela.Elements.Selected_Components;
with Gela.Elements.Selector_Names;
with Gela.Elements.Signed_Integer_Type_Definitions;
with Gela.Elements.Subtype_Declarations;
with Gela.Elements.Subtype_Indication_Or_Access_Definitions;
with Gela.Elements.Subtype_Indications;
with Gela.Elements.Subtype_Marks;
with Gela.Elements.Type_Definitions;
with Gela.Elements.Unconstrained_Array_Definitions;
with Gela.Environments;
with Gela.Plain_Type_Views;
with Gela.Profiles.Attributes;
with Gela.Profiles.Names;

package body Gela.Plain_Type_Managers is

   Universal_Access_Index  : constant Gela.Semantic_Types.Type_View_Index := 1;
   Universal_Integer_Index : constant Gela.Semantic_Types.Type_View_Index := 2;
   Universal_Real_Index    : constant Gela.Semantic_Types.Type_View_Index := 3;
   Boolean_Index           : constant Gela.Semantic_Types.Type_View_Index := 4;
   Root_Integer_Index      : constant Gela.Semantic_Types.Type_View_Index := 5;
   Root_Real_Index         : constant Gela.Semantic_Types.Type_View_Index := 6;

   -------------
   -- Boolean --
   -------------

   overriding function Boolean
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index
   is
      pragma Unreferenced (Self);
   begin
      return Boolean_Index;
   end Boolean;

   not overriding function Get
     (Self     : access Type_Manager;
      Category : Gela.Type_Categories.Category_Kinds;
      Decl     : Gela.Elements.Formal_Type_Declarations
      .Formal_Type_Declaration_Access)
        return Gela.Semantic_Types.Type_View_Index
   is
      use type Gela.Semantic_Types.Type_View_Index;

      Key : constant Back_Key := (Category, Decl);
      Pos : constant Back_Maps.Cursor := Self.Back.Find (Key);
      Result : constant Gela.Semantic_Types.Type_View_Index :=
        Self.Map.Last_Key + 1;
   begin
      if Back_Maps.Has_Element (Pos) then
         return Back_Maps.Element (Pos);
      end if;

      Self.Map.Insert
        (Result,
         Gela.Plain_Type_Views.Create_Formal_Type (Category, Decl));

      Self.Back.Insert (Key, Result);

      return Result;
   end Get;

   ---------
   -- Get --
   ---------

   not overriding function Get
     (Self     : access Type_Manager;
      Category : Gela.Type_Categories.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
      return Gela.Semantic_Types.Type_View_Index
   is
      use type Gela.Semantic_Types.Type_View_Index;

      Key : constant Back_Key := (Category, Decl);
      Pos : constant Back_Maps.Cursor := Self.Back.Find (Key);
      Result : constant Gela.Semantic_Types.Type_View_Index :=
        Self.Map.Last_Key + 1;
      Type_View : Gela.Type_Categories.Type_View_Access;
   begin
      if Back_Maps.Has_Element (Pos) then
         return Back_Maps.Element (Pos);
      end if;

      if Result in Root_Integer_Index | Root_Real_Index then
         Type_View := Gela.Plain_Type_Views.Create_Root_Type (Category, Decl);
      else
         Type_View := Gela.Plain_Type_Views.Create_Full_Type (Category, Decl);
      end if;

      Self.Map.Insert (Result, Type_View);
      Self.Back.Insert (Key, Result);

      return Result;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self  : access Type_Manager;
      Index : Gela.Semantic_Types.Type_View_Index)
      return Gela.Types.Type_View_Access
   is
      use type Gela.Semantic_Types.Type_View_Index;
   begin
      if Index = 0 then
         return null;
      else
         return Gela.Types.Type_View_Access (Self.Map.Element (Index));
      end if;
   end Get;

   not overriding function Get_Array
     (Self      : access Type_Manager;
      Category  : Gela.Type_Categories.Category_Kinds;
      Decl      : Gela.Elements.Full_Type_Declarations
                    .Full_Type_Declaration_Access;
      Component : Gela.Semantic_Types.Type_View_Index;
      Indexes   : Gela.Semantic_Types.Type_Index_Array)
      return Gela.Semantic_Types.Type_View_Index
   is
      use type Gela.Semantic_Types.Type_View_Index;

      Key : constant Back_Key := (Category, Decl);
      Pos : constant Back_Maps.Cursor := Self.Back.Find (Key);
      Result : constant Gela.Semantic_Types.Type_View_Index :=
        Self.Map.Last_Key + 1;
   begin
      if Back_Maps.Has_Element (Pos) then
         return Back_Maps.Element (Pos);
      end if;

      Self.Map.Insert
        (Result,
         Gela.Array_Type_Views.Create_Full_Type
           (Category, Decl, Component, Indexes));

      Self.Back.Insert (Key, Result);

      return Result;
   end Get_Array;

   -----------------
   -- Get_Derived --
   -----------------

   not overriding function Get_Derived
     (Self     : access Type_Manager;
      Parent   : Gela.Type_Categories.Type_View_Access;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
        return Gela.Semantic_Types.Type_View_Index
   is
      --  FIXME: Use separate maps for derived types
      use type Gela.Semantic_Types.Type_View_Index;

      Key : constant Back_Key := (Parent.Category, Decl);
      Pos : constant Back_Maps.Cursor := Self.Back.Find (Key);
      Result : constant Gela.Semantic_Types.Type_View_Index :=
        Self.Map.Last_Key + 1;
   begin
      if Back_Maps.Has_Element (Pos) then
         return Back_Maps.Element (Pos);
      end if;

      Self.Map.Insert
        (Result,
         Gela.Derived_Type_Views.Create_Derived_Type (Parent, Decl));

      Self.Back.Insert (Key, Result);

      return Result;
   end Get_Derived;

   -----------------
   -- Get_Profile --
   -----------------

   overriding function Get_Profile
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Name  : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Profiles.Profile_Access
   is
      Result : Profile_Access;
      Cursor : constant Profile_Maps.Cursor := Self.Profiles.Find (Name);
   begin
      if Profile_Maps.Has_Element (Cursor) then
         Result := Profile_Maps.Element (Cursor);
      else
         Result := new Gela.Profiles.Profile'Class'
           (Gela.Profiles.Names.Create (Env, Name));
         Self.Profiles.Insert (Name, Result);
      end if;

      return Gela.Profiles.Profile_Access (Result);
   end Get_Profile;

   -----------------
   -- Get_Profile --
   -----------------

   overriding function Get_Profile
     (Self      : access Type_Manager;
      Tipe      : Gela.Semantic_Types.Type_View_Index;
      Attribute : Gela.Lexical_Types.Symbol)
         return Gela.Profiles.Profile_Access
   is
      Result : Profile_Access;
      Key    : constant Attribute_Key := (Tipe, Attribute);
      Cursor : constant Attribute_Maps.Cursor := Self.Attributes.Find (Key);
   begin
      if Attribute_Maps.Has_Element (Cursor) then
         Result := Attribute_Maps.Element (Cursor);
      else
         case Attribute is
            when Gela.Lexical_Types.Predefined_Symbols.Ceiling |
                 Gela.Lexical_Types.Predefined_Symbols.Floor |
                 Gela.Lexical_Types.Predefined_Symbols.Fraction |
                 Gela.Lexical_Types.Predefined_Symbols.Machine |
                 Gela.Lexical_Types.Predefined_Symbols.Machine_Rounding |
                 Gela.Lexical_Types.Predefined_Symbols.Model |
                 Gela.Lexical_Types.Predefined_Symbols.Pred |
                 Gela.Lexical_Types.Predefined_Symbols.Rounding |
                 Gela.Lexical_Types.Predefined_Symbols.Succ |
                 Gela.Lexical_Types.Predefined_Symbols.Truncation |
                 Gela.Lexical_Types.Predefined_Symbols.Unbiased_Rounding =>

               Result := new Gela.Profiles.Profile'Class'
                 (Gela.Profiles.Attributes.Create
                    ((1 => Tipe), Tipe));

            when Gela.Lexical_Types.Predefined_Symbols.Pos =>

               Result := new Gela.Profiles.Profile'Class'
                 (Gela.Profiles.Attributes.Create
                    ((1 => Tipe), Self.Universal_Integer));

            when Gela.Lexical_Types.Predefined_Symbols.Mod_Symbol |
                 Gela.Lexical_Types.Predefined_Symbols.Val =>

               Result := new Gela.Profiles.Profile'Class'
                 (Gela.Profiles.Attributes.Create
                    ((1 => Self.Universal_Integer), Tipe));

            when others =>
               raise Constraint_Error;
         end case;

         Self.Attributes.Insert (Key, Result);
      end if;

      return Gela.Profiles.Profile_Access (Result);
   end Get_Profile;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Back_Key) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Key.Decl.Hash
        + Gela.Type_Categories.Category_Kinds'Pos (Key.Category);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Ada.Containers.Hash_Type is
   begin
      return Self.Hash;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self : Gela.Elements.Root_Type_Definitions.
        Root_Type_Definition_Access)
      return Ada.Containers.Hash_Type is
   begin
      return Self.Hash;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Attribute_Key) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Ada.Containers.Hash_Type (Value.Tipe) * 2017
        + Gela.Lexical_Types.Symbol'Pos (Value.Attribute);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   package S renames Standard;

   procedure Initialize
     (Self     : access Type_Manager;
      Standard : Gela.Elements.Element_Access)
   is
      procedure Create
        (Category : Gela.Type_Categories.Category_Kinds;
         Index    : Gela.Semantic_Types.Type_View_Index);

      procedure Find_Type
        (Symbol   : Gela.Lexical_Types.Symbol;
         Category : Gela.Type_Categories.Category_Kinds;
         Expect   : Gela.Semantic_Types.Type_View_Index);

      Comp : constant Gela.Compilations.Compilation_Access :=
        Standard.Enclosing_Compilation;
      Factory : constant Gela.Element_Factories.Element_Factory_Access :=
        Comp.Factory;

      procedure Create
        (Category : Gela.Type_Categories.Category_Kinds;
         Index    : Gela.Semantic_Types.Type_View_Index)
      is
         Id   : Gela.Elements.Defining_Identifiers.Defining_Identifier_Access;
         Def  : Gela.Elements.Root_Type_Definitions
                  .Root_Type_Definition_Access;
         Node : Gela.Elements.Full_Type_Declarations
                  .Full_Type_Declaration_Access;
      begin
         Id := Factory.Defining_Identifier (Identifier_Token => 0);

         Def := Factory.Root_Type_Definition (0);
         Self.Roots.Insert (Def, Index);

         Node := Factory.Full_Type_Declaration
           (Type_Token            => 0,
            Names                 => Id,
            Discriminant_Part     => null,
            Is_Token              => 0,
            Type_Declaration_View =>
              Gela.Elements.Type_Definitions.Type_Definition_Access (Def),
            Aspect_Specifications => Factory.Aspect_Specification_Sequence,
            Semicolon_Token       => 0);

         Self.Map.Insert
           (Index,
            Gela.Plain_Type_Views.Create_Full_Type (Category, Node));
      end Create;

      ---------------
      -- Find_Type --
      ---------------

      procedure Find_Type
        (Symbol   : Gela.Lexical_Types.Symbol;
         Category : Gela.Type_Categories.Category_Kinds;
         Expect   : Gela.Semantic_Types.Type_View_Index)
      is
         package Visiters is
            type Visiter is new Gela.Element_Visiters.Visiter with record
               Stop   : S.Boolean := False;
               Result : Gela.Semantic_Types.Type_View_Index := 0;
            end record;

            overriding procedure Full_Type_Declaration
              (Self : in out Visiter;
               Node : not null Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access);
         end Visiters;

         package body Visiters is

            overriding procedure Full_Type_Declaration
              (Self : in out Visiter;
               Node : not null Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access)
            is
               use type Gela.Lexical_Types.Symbol;
               Identifier : constant Gela.Elements.Defining_Identifiers
                 .Defining_Identifier_Access := Node.Names;
               Name : constant
                 Gela.Elements.Defining_Names.Defining_Name_Access :=
                   Gela.Elements.Defining_Names.Defining_Name_Access
                     (Identifier);
               Token : constant Gela.Lexical_Types.Token :=
                 Comp.Get_Token (Identifier.Identifier_Token);
            begin
               if Token.Symbol = Symbol then
                  Self.Stop := True;
                  Self.Result := Initialize.Self.Get
                    (Category => Category,
                     Decl     => Node);

                  if Category in Gela.Type_Categories.A_Boolean then
                     Initialize.Self.Boolean := Name;
                  end if;
               end if;
            end Full_Type_Declaration;
         end Visiters;

         use type Gela.Semantic_Types.Type_View_Index;
         Seq : constant Gela.Elements.Basic_Declarative_Items
           .Basic_Declarative_Item_Sequence_Access
             := Gela.Elements.Package_Declarations
                 .Package_Declaration_Access (Standard)
                   .Visible_Part_Declarative_Items;
         Cursor : Gela.Elements.Basic_Declarative_Items
           .Basic_Declarative_Item_Sequence_Cursor := Seq.First;
         Visiter : aliased Visiters.Visiter;
      begin
         while Cursor.Has_Element and not Visiter.Stop loop
            Cursor.Element.Visit (Visiter);
            Cursor.Next;
         end loop;

         pragma Assert (Visiter.Result = Expect);
      end Find_Type;

      use Gela.Type_Categories;
   begin
      Create (An_Universal_Access, Universal_Access_Index);
      Create (An_Universal_Integer, Universal_Integer_Index);
      Create (An_Universal_Real, Universal_Real_Index);
      Find_Type
        (Gela.Lexical_Types.Predefined_Symbols.Boolean,
         Gela.Type_Categories.A_Boolean,
         Boolean_Index);
      Find_Type
        (Gela.Lexical_Types.Predefined_Symbols.Root_Integer,
         Gela.Type_Categories.A_Signed_Integer,
         Root_Integer_Index);
      Find_Type
        (Gela.Lexical_Types.Predefined_Symbols.Root_Real,
         Gela.Type_Categories.A_Float_Point,
         Root_Real_Index);
   end Initialize;

   ------------------
   -- Root_Integer --
   ------------------

   overriding function Root_Integer
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index
   is
      pragma Unreferenced (Self);
   begin
      return Root_Integer_Index;
   end Root_Integer;

   ---------------
   -- Root_Real --
   ---------------

   overriding function Root_Real
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index
   is
      pragma Unreferenced (Self);
   begin
      return Root_Real_Index;
   end Root_Real;

   ------------------
   -- Type_By_Name --
   ------------------

   overriding function Type_By_Name
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Defining_Names.Defining_Name_Access)
        return Gela.Semantic_Types.Type_View_Index
   is
      ES : constant Gela.Environments.Environment_Set_Access :=
        Self.Context.Environment_Set;
      Completions : constant Gela.Environments.Completion_List
        := ES.Completions (Env, Node);

      Decl : Gela.Elements.Element_Access;
   begin
      if Completions.Length > 0 then
         Decl := Completions.Data (1).Enclosing_Element;
      else
         Decl := Node.Enclosing_Element;
      end if;

      return Self.Type_From_Declaration (Env, Decl);
   end Type_By_Name;

   ---------------------------
   -- Type_From_Declaration --
   ---------------------------

   overriding function Type_From_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_View_Index
   is

      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result  : Gela.Semantic_Types.Type_View_Index := 0;
            Boolean : Gela.Elements.Defining_Names.Defining_Name_Access;
         end record;

         overriding procedure Access_To_Object_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Access_To_Object_Definitions.
              Access_To_Object_Definition_Access);

         overriding procedure Constrained_Array_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Constrained_Array_Definitions.
              Constrained_Array_Definition_Access);

         overriding procedure Derived_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Derived_Type_Definitions.
              Derived_Type_Definition_Access);

         overriding procedure Enumeration_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Enumeration_Type_Definitions.
              Enumeration_Type_Definition_Access);

         overriding procedure Floating_Point_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Floating_Point_Definitions.
              Floating_Point_Definition_Access);

         overriding procedure Formal_Discrete_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Discrete_Type_Definitions.
              Formal_Discrete_Type_Definition_Access);

         overriding procedure Formal_Signed_Integer_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.
              Formal_Signed_Integer_Type_Definitions.
                Formal_Signed_Integer_Type_Definition_Access);

         overriding procedure Formal_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Type_Declarations.
              Formal_Type_Declaration_Access);

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access);

         overriding procedure Record_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Type_Definitions.
              Record_Type_Definition_Access);

         overriding procedure Root_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Root_Type_Definitions.
              Root_Type_Definition_Access);

         overriding procedure Signed_Integer_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Signed_Integer_Type_Definitions.
              Signed_Integer_Type_Definition_Access);

         overriding procedure Subtype_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subtype_Declarations.
              Subtype_Declaration_Access);

         overriding procedure Unconstrained_Array_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Unconstrained_Array_Definitions.
              Unconstrained_Array_Definition_Access);

      end Visiters;

      package Is_Char_Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Found : Standard.Boolean := False;
         end record;

         overriding procedure Defining_Character_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Character_Literals.
              Defining_Character_Literal_Access);

      end Is_Char_Visiters;

      ----------------------
      -- Is_Char_Visiters --
      ----------------------

      package body Is_Char_Visiters is

         overriding procedure Defining_Character_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Character_Literals.
              Defining_Character_Literal_Access)
         is
            pragma Unreferenced (Node);
         begin
            Self.Found := True;
         end Defining_Character_Literal;

      end Is_Char_Visiters;

      --------------
      -- Visiters --
      --------------

      package body Visiters is

         overriding procedure Access_To_Object_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Access_To_Object_Definitions.
              Access_To_Object_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Get
              (Category => Gela.Type_Categories.A_Variable_Access,
               Decl     => Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access (Node.Enclosing_Element));
         end Access_To_Object_Definition;

         overriding procedure Derived_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Derived_Type_Definitions.
              Derived_Type_Definition_Access)
         is
            use type Gela.Semantic_Types.Type_View_Index;

            Parent : constant Gela.Elements.Subtype_Indications.
              Subtype_Indication_Access := Node.Parent_Subtype_Indication;
            Tipe : constant Gela.Semantic_Types.Type_View_Index :=
              Type_From_Declaration.Self.Type_From_Subtype_Indication
                (Env, Parent);
            Type_View : Gela.Type_Categories.Type_View_Access;
         begin
            if Tipe /= 0 then
               Type_View := Gela.Type_Categories.Type_View_Access
                 (Type_From_Declaration.Self.Get (Tipe));

               Self.Result := Type_From_Declaration.Self.Get_Derived
                 (Parent   => Type_View,
                  Decl     => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element));
            end if;
         end Derived_Type_Definition;

         overriding procedure Enumeration_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Enumeration_Type_Definitions.
              Enumeration_Type_Definition_Access)
         is
            Enum : Gela.Elements.Defining_Enumeration_Names.
              Defining_Enumeration_Name_Access;
            Enums : constant Gela.Elements.Enumeration_Literal_Specifications.
              Enumeration_Literal_Specification_Sequence_Access :=
                Node.Enumeration_Literal_Declarations;
            Cursor : Gela.Elements.Enumeration_Literal_Specifications.
              Enumeration_Literal_Specification_Sequence_Cursor := Enums.First;

            V : Is_Char_Visiters.Visiter;
         begin
            while Cursor.Has_Element loop
               Enum := Cursor.Element.Names;
               Enum.Visit (V);
               exit when V.Found;

               Cursor.Next;
            end loop;

            if V.Found then
               Self.Result := Type_From_Declaration.Self.Get
                 (Category => Gela.Type_Categories.A_Character,
                  Decl     => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element));
            else
               Self.Result := Type_From_Declaration.Self.Get
                 (Category => Gela.Type_Categories.An_Other_Enum,
                  Decl     => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element));
            end if;
         end Enumeration_Type_Definition;

         overriding procedure Floating_Point_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Floating_Point_Definitions.
              Floating_Point_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Get
              (Category => Gela.Type_Categories.A_Float_Point,
               Decl     => Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access (Node.Enclosing_Element));
         end Floating_Point_Definition;

         overriding procedure Formal_Discrete_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Discrete_Type_Definitions.
              Formal_Discrete_Type_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Get
              (Category => Gela.Type_Categories.An_Other_Enum,
               Decl     => Gela.Elements.Formal_Type_Declarations.
                 Formal_Type_Declaration_Access (Node.Enclosing_Element));
         end Formal_Discrete_Type_Definition;

         overriding procedure Formal_Signed_Integer_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.
              Formal_Signed_Integer_Type_Definitions.
                Formal_Signed_Integer_Type_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Get
              (Category => Gela.Type_Categories.A_Signed_Integer,
               Decl     => Gela.Elements.Formal_Type_Declarations.
                 Formal_Type_Declaration_Access (Node.Enclosing_Element));
         end Formal_Signed_Integer_Type_Definition;

         overriding procedure Formal_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Type_Declarations.
              Formal_Type_Declaration_Access)
         is
            View : constant Gela.Elements.Formal_Type_Definitions.
              Formal_Type_Definition_Access := Node.Type_Declaration_View;
         begin
            View.Visit (Self);
         end Formal_Type_Declaration;

         ---------------------------
         -- Full_Type_Declaration --
         ---------------------------

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access)
         is
            use type Gela.Elements.Defining_Names.Defining_Name_Access;

            Name : constant Gela.Elements.Defining_Names.Defining_Name_Access
              := Gela.Elements.Defining_Names.Defining_Name_Access
                (Node.Names);
            View : constant Gela.Elements.Type_Definitions.
              Type_Definition_Access := Node.Type_Declaration_View;
         begin
            if Name = Self.Boolean then
               Self.Result := Type_From_Declaration.Self.Get
                 (Category => Gela.Type_Categories.A_Boolean,
                  Decl     => Node);
            else
               View.Visit (Self);
            end if;
         end Full_Type_Declaration;

         ----------------------------
         -- Record_Type_Definition --
         ----------------------------

         overriding procedure Record_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Type_Definitions.
              Record_Type_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Get
              (Category => Gela.Type_Categories.A_Untagged_Record,
               Decl     => Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access (Node.Enclosing_Element));
         end Record_Type_Definition;

         --------------------------
         -- Root_Type_Definition --
         --------------------------

         overriding procedure Root_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Root_Type_Definitions.
              Root_Type_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Roots.Element (Node);
         end Root_Type_Definition;

         overriding procedure Signed_Integer_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Signed_Integer_Type_Definitions.
              Signed_Integer_Type_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Get
              (Category => Gela.Type_Categories.A_Signed_Integer,
               Decl     => Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access (Node.Enclosing_Element));
         end Signed_Integer_Type_Definition;

         overriding procedure Subtype_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subtype_Declarations.
              Subtype_Declaration_Access)
         is
            Indication : constant Gela.Elements.Subtype_Indications.
              Subtype_Indication_Access := Node.Type_Declaration_View;
         begin
            Self.Result :=
              Type_From_Declaration.Self.Type_From_Subtype_Indication
                (Env, Indication);
         end Subtype_Declaration;

         overriding procedure Unconstrained_Array_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Unconstrained_Array_Definitions.
              Unconstrained_Array_Definition_Access)
         is
            use type Gela.Type_Categories.Category_Kinds;

            Component : constant Gela.Elements.Component_Definitions.
              Component_Definition_Access := Node.Array_Component_Definition;

            Component_Type : constant Gela.Semantic_Types.Type_View_Index :=
              Type_From_Declaration.Self.Type_Of_Object_Declaration
                (Env, Gela.Elements.Element_Access (Component));

            Component_Type_View : constant Gela.Types.Type_View_Access :=
              Type_From_Declaration.Self.Get (Component_Type);

            Index_Seq : constant Gela.Elements.Subtype_Marks.
              Subtype_Mark_Sequence_Access := Node.Index_Subtype_Definitions;

            Indexes  : Gela.Semantic_Types.Type_Index_Array
              (1 .. Index_Seq.Length);
         begin
            declare
               Index  : Positive := 1;
               Cursor : Gela.Elements.Subtype_Marks
                 .Subtype_Mark_Sequence_Cursor := Index_Seq.First;
            begin
               while Cursor.Has_Element loop
                  Indexes (Index) :=
                    Type_From_Declaration.Self.Type_From_Subtype_Mark
                      (Env, Cursor.Element);

                  Index := Index + 1;
                  Cursor.Next;
               end loop;
            end;

            if Component_Type_View.Assigned and then
              Component_Type_View.Is_Character
            then
               Self.Result := Type_From_Declaration.Self.Get_Array
                 (Category  => Gela.Type_Categories.A_String,
                  Decl      => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element),
                  Component => Component_Type,
                  Indexes   => Indexes);
            else
               Self.Result := Type_From_Declaration.Self.Get_Array
                 (Category  => Gela.Type_Categories.An_Other_Array,
                  Decl      => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element),
                  Component => Component_Type,
                  Indexes   => Indexes);
            end if;
         end Unconstrained_Array_Definition;

         overriding procedure Constrained_Array_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Constrained_Array_Definitions.
              Constrained_Array_Definition_Access)
         is
            use type Gela.Type_Categories.Category_Kinds;

            Component : constant Gela.Elements.Component_Definitions.
              Component_Definition_Access := Node.Array_Component_Definition;

            Component_Type : constant Gela.Semantic_Types.Type_View_Index :=
              Type_From_Declaration.Self.Type_Of_Object_Declaration
                (Env, Gela.Elements.Element_Access (Component));

            Component_Type_View : constant Gela.Types.Type_View_Access :=
              Type_From_Declaration.Self.Get (Component_Type);

            Index_Seq : constant Gela.Elements.Discrete_Subtype_Definitions.
              Discrete_Subtype_Definition_Sequence_Access :=
                Node.Discrete_Subtype_Definitions;

            Indexes  : Gela.Semantic_Types.Type_Index_Array
              (1 .. Index_Seq.Length);
         begin
            if Node.Env_In in 0 then
               --  FIXME Drop this when generic instance issue is resolved
               return;
            end if;

            declare
               Index  : Positive := 1;
               Element : Gela.Elements.Discrete_Subtype_Definitions
                 .Discrete_Subtype_Definition_Access;
               Cursor : Gela.Elements.Discrete_Subtype_Definitions
                 .Discrete_Subtype_Definition_Sequence_Cursor :=
                   Index_Seq.First;
            begin
               while Cursor.Has_Element loop
                  Element := Cursor.Element;
                  Indexes (Index) :=
                    Type_From_Declaration.Self.Type_From_Discrete_Subtype
                      (Element.Env_In, Element);

                  Index := Index + 1;
                  Cursor.Next;
               end loop;
            end;

            if Component_Type_View.Assigned and then
              Component_Type_View.Is_Character
            then
               Self.Result := Type_From_Declaration.Self.Get_Array
                 (Category  => Gela.Type_Categories.A_String,
                  Decl      => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element),
                  Component => Component_Type,
                  Indexes   => Indexes);
            else
               Self.Result := Type_From_Declaration.Self.Get_Array
                 (Category  => Gela.Type_Categories.An_Other_Array,
                  Decl      => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element),
                  Component => Component_Type,
                  Indexes   => Indexes);
            end if;
         end Constrained_Array_Definition;

      end Visiters;

      V : Visiters.Visiter := (0, Self.Boolean);
   begin
      Node.Visit (V);

      return V.Result;
   end Type_From_Declaration;

   --------------------------------
   -- Type_From_Discrete_Subtype --
   --------------------------------

   overriding function Type_From_Discrete_Subtype
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Discrete_Subtype_Definitions.
                Discrete_Subtype_Definition'Class)
        return Gela.Semantic_Types.Type_View_Index
   is
      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Type_View_Index := 0;
         end record;

         overriding procedure Discrete_Simple_Expression_Range
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discrete_Simple_Expression_Ranges.
              Discrete_Simple_Expression_Range_Access);

         overriding procedure Discrete_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discrete_Subtype_Indications.
              Discrete_Subtype_Indication_Access);

      end Visiters;

      package body Visiters is

         overriding procedure Discrete_Simple_Expression_Range
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discrete_Simple_Expression_Ranges.
              Discrete_Simple_Expression_Range_Access) is
         begin
            Self.Result := Node.Type_Index;
         end Discrete_Simple_Expression_Range;

         overriding procedure Discrete_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discrete_Subtype_Indications.
              Discrete_Subtype_Indication_Access) is
         begin
            Self.Result := Type_From_Discrete_Subtype.Self.
              Type_From_Subtype_Mark (Env, Node.Subtype_Mark);
         end Discrete_Subtype_Indication;

      end Visiters;

      V : Visiters.Visiter;
   begin
      Node.Visit (V);

      return V.Result;
   end Type_From_Discrete_Subtype;

   overriding function Type_From_Subtype_Indication
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Object_Definitions.Object_Definition'Class)
      return Gela.Semantic_Types.Type_View_Index
   is

      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result  : Gela.Semantic_Types.Type_View_Index := 0;
         end record;

         overriding procedure Composite_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Subtype_Indications.
              Composite_Subtype_Indication_Access);

         overriding procedure Scalar_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Scalar_Subtype_Indications.
              Scalar_Subtype_Indication_Access);

      end Visiters;

      --------------
      -- Visiters --
      --------------

      package body Visiters is

         overriding procedure Composite_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Subtype_Indications.
              Composite_Subtype_Indication_Access) is
         begin
            Self.Result := Type_From_Subtype_Indication.Self.
              Type_From_Subtype_Mark (Env, Node.Subtype_Mark);
         end Composite_Subtype_Indication;

         overriding procedure Scalar_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Scalar_Subtype_Indications.
              Scalar_Subtype_Indication_Access) is
         begin
            Self.Result := Type_From_Subtype_Indication.Self.
              Type_From_Subtype_Mark (Env, Node.Subtype_Mark);
         end Scalar_Subtype_Indication;

      end Visiters;

      V : Visiters.Visiter := (Result => 0);
   begin
      Node.Visit (V);

      return V.Result;
   end Type_From_Subtype_Indication;

   ----------------------------
   -- Type_From_Subtype_Mark --
   ----------------------------

   overriding function Type_From_Subtype_Mark
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : access Gela.Elements.Subtype_Mark_Or_Access_Definitions.
                Subtype_Mark_Or_Access_Definition'Class)
      return Gela.Semantic_Types.Type_View_Index
   is
      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Type_View_Index := 0;
         end record;

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access);

         overriding procedure Selected_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Components.
              Selected_Component_Access);

      end Visiters;

      package body Visiters is

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access)
         is
            View          : Gela.Elements.Subtype_Marks.Subtype_Mark_Access;
            Defining_Name : constant Gela.Elements.Defining_Names.
              Defining_Name_Access := Node.Defining_Name;
         begin
            if not Defining_Name.Assigned then
               return;
            end if;

            View := Gela.Elements.Subtype_Marks.Subtype_Mark_Access
              (Defining_Name.Corresponding_View);

            if View.Assigned then
               Self.Result := Type_From_Subtype_Mark.Self.
                 Type_From_Subtype_Mark (Env, View);
            else
               Self.Result :=
                 Type_From_Subtype_Mark.Self.Type_By_Name (Env, Defining_Name);
            end if;
         end Identifier;

         overriding procedure Selected_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Components.
              Selected_Component_Access)
         is
            Selector : constant Gela.Elements.Selector_Names.
              Selector_Name_Access := Node.Selector;
         begin
            Selector.Visit (Self);
         end Selected_Component;

      end Visiters;

      V    : Visiters.Visiter;
   begin
      Node.Visit (V);

      return V.Result;
   end Type_From_Subtype_Mark;

   --------------------------------
   -- Type_Of_Object_Declaration --
   --------------------------------

   overriding function Type_Of_Object_Declaration
     (Self  : access Type_Manager;
      Env   : Gela.Semantic_Types.Env_Index;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_View_Index
   is
      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Type_View_Index := 0;
         end record;

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access);

         overriding procedure Component_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Definitions.
              Component_Definition_Access);

         overriding procedure Discriminant_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Access);

         overriding procedure Enumeration_Literal_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Enumeration_Literal_Specifications.
              Enumeration_Literal_Specification_Access);

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access);

         overriding procedure Loop_Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Loop_Parameter_Specifications.
              Loop_Parameter_Specification_Access);

         overriding procedure Number_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Number_Declarations.
              Number_Declaration_Access);

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access);

         overriding procedure Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Access);

      end Visiters;

      package body Visiters is

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access)
         is
            X : constant Gela.Elements.Component_Definitions.
              Component_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            X.Visit (Self);
         end Component_Declaration;

         overriding procedure Component_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Definitions.
              Component_Definition_Access)
         is
            X : constant Gela.Elements.Subtype_Indication_Or_Access_Definitions
              .Subtype_Indication_Or_Access_Definition_Access :=
                Node.Component_Subtype_Indication;
         begin
            Self.Result :=
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Indication
                (Env,
                 Gela.Elements.Object_Definitions.Object_Definition_Access
                   (X));
         end Component_Definition;

         overriding procedure Discriminant_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Access)
         is
            X : constant Gela.Elements.Subtype_Mark_Or_Access_Definitions.
              Subtype_Mark_Or_Access_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            Self.Result :=
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Mark (Env, X);
         end Discriminant_Specification;

         overriding procedure Enumeration_Literal_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Enumeration_Literal_Specifications.
              Enumeration_Literal_Specification_Access)
         is
            Def : constant Gela.Elements.Element_Access :=
              Node.Enclosing_Element;
            Decl : constant Gela.Elements.Element_Access :=
              Def.Enclosing_Element;
         begin
            Self.Result := Type_Of_Object_Declaration.Self.
              Type_From_Declaration (Env, Decl);
         end Enumeration_Literal_Specification;

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access)
         is
            X : constant Gela.Elements.Subtype_Mark_Or_Access_Definitions.
              Subtype_Mark_Or_Access_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            Self.Result :=
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Mark (Env, X);
         end Formal_Object_Declaration;

         overriding procedure Loop_Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Loop_Parameter_Specifications.
              Loop_Parameter_Specification_Access) is
         begin
            Self.Result :=
              Type_Of_Object_Declaration.Self.Type_From_Discrete_Subtype
                (Env, Node.Specification_Subtype_Definition);
         end Loop_Parameter_Specification;

         overriding procedure Number_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Number_Declarations.
              Number_Declaration_Access)
         is
            pragma Unreferenced (Node);
         begin
            --  FIXME!
            Self.Result := Type_Of_Object_Declaration.Self.Universal_Integer;
         end Number_Declaration;

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access)
         is
            X : constant Gela.Elements.Object_Definitions.
              Object_Definition_Access := Node.Object_Declaration_Subtype;
         begin
            Self.Result :=
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Indication
                (Env, X);
         end Object_Declaration;

         overriding procedure Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Access)
         is
            X : constant Gela.Elements.Subtype_Mark_Or_Access_Definitions.
              Subtype_Mark_Or_Access_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            Self.Result :=
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Mark (Env, X);
         end Parameter_Specification;

      end Visiters;

      V    : Visiters.Visiter;
   begin
      Node.Visit (V);

      return V.Result;
   end Type_Of_Object_Declaration;

   ----------------------
   -- Universal_Access --
   ----------------------

   overriding function Universal_Access
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index
   is
      pragma Unreferenced (Self);
   begin
      return Universal_Access_Index;
   end Universal_Access;

   -----------------------
   -- Universal_Integer --
   -----------------------

   overriding function Universal_Integer
     (Self  : access Type_Manager)
      return Gela.Semantic_Types.Type_View_Index
   is
      pragma Unreferenced (Self);
   begin
      return Universal_Integer_Index;
   end Universal_Integer;

   --------------------
   -- Universal_Real --
   --------------------

   overriding function Universal_Real
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_View_Index
   is
      pragma Unreferenced (Self);
   begin
      return Universal_Real_Index;
   end Universal_Real;

end Gela.Plain_Type_Managers;
