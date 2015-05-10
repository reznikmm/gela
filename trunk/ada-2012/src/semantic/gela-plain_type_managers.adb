with Gela.Compilations;
with Gela.Element_Factories;
with Gela.Element_Visiters;
with Gela.Elements.Access_To_Object_Definitions;
with Gela.Elements.Component_Declarations;
with Gela.Elements.Component_Definitions;
with Gela.Elements.Defining_Character_Literals;
with Gela.Elements.Defining_Enumeration_Names;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Derived_Type_Definitions;
with Gela.Elements.Discriminant_Specifications;
with Gela.Elements.Enumeration_Literal_Specifications;
with Gela.Elements.Enumeration_Type_Definitions;
with Gela.Elements.Floating_Point_Definitions;
with Gela.Elements.Identifiers;
with Gela.Elements.Object_Declarations;
with Gela.Elements.Object_Definitions;
with Gela.Elements.Parameter_Specifications;
with Gela.Elements.Record_Type_Definitions;
with Gela.Elements.Root_Type_Definitions;
with Gela.Elements.Selected_Components;
with Gela.Elements.Selector_Names;
with Gela.Elements.Signed_Integer_Type_Definitions;
with Gela.Elements.Subtype_Declarations;
with Gela.Elements.Subtype_Indication_Or_Access_Definitions;
with Gela.Elements.Subtype_Indications;
with Gela.Elements.Subtype_Marks;
with Gela.Elements.Type_Definitions;
with Gela.Elements.Unconstrained_Array_Definitions;
with Gela.Plain_Type_Views;
with Gela.Profiles.Names;

package body Gela.Plain_Type_Managers is

   Universal_Access_Index  : constant Gela.Semantic_Types.Type_Index := 1;
   Universal_Integer_Index : constant Gela.Semantic_Types.Type_Index := 2;
   Universal_Real_Index    : constant Gela.Semantic_Types.Type_Index := 3;

   ---------
   -- Get --
   ---------

   not overriding function Get
     (Self     : access Type_Manager;
      Category : Gela.Type_Views.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
      return Gela.Semantic_Types.Type_Index
   is
      use type Gela.Semantic_Types.Type_Index;

      Key : constant Back_Key := (Category, Decl);
      Pos : constant Back_Maps.Cursor := Self.Back.Find (Key);
      Result : constant Gela.Semantic_Types.Type_Index :=
        Self.Map.Last_Key + 1;
   begin
      if Back_Maps.Has_Element (Pos) then
         return Back_Maps.Element (Pos);
      end if;

      Self.Map.Insert
        (Result,
         Gela.Plain_Type_Views.Create_Full_Type (Category, Decl));

      Self.Back.Insert (Key, Result);

      return Result;
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self  : access Type_Manager;
      Index : Gela.Semantic_Types.Type_Index)
      return Gela.Type_Views.Type_View_Access
   is
      use type Gela.Semantic_Types.Type_Index;
   begin
      if Index = 0 then
         return null;
      else
         return Self.Map.Element (Index);
      end if;
   end Get;

   -----------------
   -- Get_Profile --
   -----------------

   overriding function Get_Profile
     (Self  : access Type_Manager;
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
           (Gela.Profiles.Names.Create (Name));
         Self.Profiles.Insert (Name, Result);
      end if;

      return Gela.Profiles.Profile_Access (Result);
   end Get_Profile;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Back_Key) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Key.Decl.Hash + Gela.Type_Views.Category_Kinds'Pos (Key.Category);
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : access Type_Manager;
      Standard : Gela.Elements.Element_Access)
   is
      procedure Create
        (Category : Gela.Type_Views.Category_Kinds;
         Index    : Gela.Semantic_Types.Type_Index);

      Comp : constant Gela.Compilations.Compilation_Access :=
        Standard.Enclosing_Compilation;
      Factory : constant Gela.Element_Factories.Element_Factory_Access :=
        Comp.Factory;

      procedure Create
        (Category : Gela.Type_Views.Category_Kinds;
         Index    : Gela.Semantic_Types.Type_Index)
      is
         Id   : Gela.Elements.Defining_Identifiers.Defining_Identifier_Access;
         Def  : Gela.Elements.Root_Type_Definitions
                  .Root_Type_Definition_Access;
         Node : Gela.Elements.Full_Type_Declarations
                  .Full_Type_Declaration_Access;
      begin
         Id := Factory.Defining_Identifier (Identifier_Token => 0);

         Def := Factory.Root_Type_Definition (0);
         Def.Set_Type_Kind (Index);

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

   begin
      Create (Gela.Type_Views.An_Universal_Access, Universal_Access_Index);
      Create (Gela.Type_Views.An_Universal_Integer, Universal_Integer_Index);
      Create (Gela.Type_Views.An_Universal_Real, Universal_Real_Index);
   end Initialize;

   ------------------
   -- Type_By_Name --
   ------------------

   overriding function Type_By_Name
     (Self  : access Type_Manager;
      Node  : Gela.Elements.Defining_Names.Defining_Name_Access)
        return Gela.Semantic_Types.Type_Index
   is
      Decl : constant Gela.Elements.Element_Access := Node.Enclosing_Element;
   begin
      return Self.Type_From_Declaration (Decl);
   end Type_By_Name;

   ---------------------------
   -- Type_From_Declaration --
   ---------------------------

   overriding function Type_From_Declaration
     (Self  : access Type_Manager;
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_Index
   is

      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Type_Index := 0;
         end record;

         overriding procedure Access_To_Object_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Access_To_Object_Definitions.
              Access_To_Object_Definition_Access);

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
            Found : Boolean := False;
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
              (Category => Gela.Type_Views.A_Variable_Access,
               Decl     => Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access (Node.Enclosing_Element));
         end Access_To_Object_Definition;

         overriding procedure Derived_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Derived_Type_Definitions.
              Derived_Type_Definition_Access)
         is
            use type Gela.Semantic_Types.Type_Index;

            Parent : constant Gela.Elements.Subtype_Indications.
              Subtype_Indication_Access := Node.Parent_Subtype_Indication;
            Subtype_Mark : constant Gela.Elements.Subtype_Marks
              .Subtype_Mark_Access  := Parent.Subtype_Mark;
            Tipe : constant Gela.Semantic_Types.Type_Index :=
              Type_From_Declaration.Self.Type_From_Subtype_Mark (Subtype_Mark);
            Type_View : Gela.Type_Views.Type_View_Access;
         begin
            if Tipe /= 0 then
               Type_View := Type_From_Declaration.Self.Get (Tipe);

               Self.Result := Type_From_Declaration.Self.Get
                 (Category => Type_View.Category,
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
                 (Category => Gela.Type_Views.A_Character,
                  Decl     => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element));
            else
               Self.Result := Type_From_Declaration.Self.Get
                 (Category => Gela.Type_Views.An_Other_Enum,
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
              (Category => Gela.Type_Views.A_Float_Point,
               Decl     => Gela.Elements.Full_Type_Declarations.
                 Full_Type_Declaration_Access (Node.Enclosing_Element));
         end Floating_Point_Definition;

         ---------------------------
         -- Full_Type_Declaration --
         ---------------------------

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access)
         is
            View : constant Gela.Elements.Type_Definitions.
              Type_Definition_Access := Node.Type_Declaration_View;
         begin
            View.Visit (Self);
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
              (Category => Gela.Type_Views.A_Untagged_Record,
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
            Self.Result := Node.Type_Kind;
         end Root_Type_Definition;

         overriding procedure Signed_Integer_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Signed_Integer_Type_Definitions.
              Signed_Integer_Type_Definition_Access) is
         begin
            Self.Result := Type_From_Declaration.Self.Get
              (Category => Gela.Type_Views.A_Signed_Integer,
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
            Subtype_Mark : constant Gela.Elements.Subtype_Marks
              .Subtype_Mark_Access  := Indication.Subtype_Mark;
         begin
            Self.Result := Type_From_Declaration.Self.Type_From_Subtype_Mark
              (Subtype_Mark);
         end Subtype_Declaration;

         overriding procedure Unconstrained_Array_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Unconstrained_Array_Definitions.
              Unconstrained_Array_Definition_Access)
         is
            use type Gela.Type_Views.Category_Kinds;

            Component : constant Gela.Elements.Component_Definitions.
              Component_Definition_Access := Node.Array_Component_Definition;

            Component_Type : constant Gela.Semantic_Types.Type_Index :=
              Type_From_Declaration.Self.Type_Of_Object_Declaration
                (Gela.Elements.Element_Access (Component));

            Component_Type_View : constant Gela.Type_Views.Type_View_Access :=
              Type_From_Declaration.Self.Get (Component_Type);
         begin
            if Component_Type_View.Assigned and then
              Component_Type_View.Category = Gela.Type_Views.A_Character
            then
               Self.Result := Type_From_Declaration.Self.Get
                 (Category => Gela.Type_Views.A_String,
                  Decl     => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element));
            else
               Self.Result := Type_From_Declaration.Self.Get
                 (Category => Gela.Type_Views.An_Other_Array,
                  Decl     => Gela.Elements.Full_Type_Declarations.
                    Full_Type_Declaration_Access (Node.Enclosing_Element));
            end if;
         end Unconstrained_Array_Definition;

      end Visiters;

      V : Visiters.Visiter;
   begin
      Node.Visit (V);

      return V.Result;
   end Type_From_Declaration;

   ----------------------------
   -- Type_From_Subtype_Mark --
   ----------------------------

   overriding function Type_From_Subtype_Mark
     (Self  : access Type_Manager;
      Node  : access Gela.Elements.Subtype_Mark_Or_Access_Definitions.
                Subtype_Mark_Or_Access_Definition'Class)
      return Gela.Semantic_Types.Type_Index
   is
      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Type_Index := 0;
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
            Defining_Name : constant Gela.Elements.Defining_Names.
              Defining_Name_Access := Node.Defining_Name;
         begin
            if Defining_Name.Assigned then
               Self.Result :=
                 Type_From_Subtype_Mark.Self.Type_From_Declaration
                   (Defining_Name.Enclosing_Element);
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
      Node  : Gela.Elements.Element_Access)
      return Gela.Semantic_Types.Type_Index
   is
      package Visiters is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Semantic_Types.Type_Index := 0;
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

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access);

         overriding procedure Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Access);

         overriding procedure Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subtype_Indications.
              Subtype_Indication_Access);

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
            X.Visit (Self);
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
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Mark (X);
         end Discriminant_Specification;

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access)
         is
            X : constant Gela.Elements.Object_Definitions.
              Object_Definition_Access := Node.Object_Declaration_Subtype;
         begin
            X.Visit (Self);
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
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Mark (X);
         end Parameter_Specification;

         overriding procedure Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subtype_Indications.
              Subtype_Indication_Access)
         is
            X : constant Gela.Elements.Subtype_Marks.Subtype_Mark_Access  :=
              Node.Subtype_Mark;
         begin
            Self.Result :=
              Type_Of_Object_Declaration.Self.Type_From_Subtype_Mark (X);
         end Subtype_Indication;
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
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_Index
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
      return Gela.Semantic_Types.Type_Index
   is
      pragma Unreferenced (Self);
   begin
      return Universal_Integer_Index;
   end Universal_Integer;

   --------------------
   -- Universal_Real --
   --------------------

   overriding function Universal_Real
     (Self  : access Type_Manager) return Gela.Semantic_Types.Type_Index
   is
      pragma Unreferenced (Self);
   begin
      return Universal_Real_Index;
   end Universal_Real;

end Gela.Plain_Type_Managers;
