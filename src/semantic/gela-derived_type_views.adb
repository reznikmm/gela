with Gela.Types.Discriminated;

with Gela.Elements.Defining_Identifiers;
pragma Unreferenced (Gela.Elements.Defining_Identifiers);

package body Gela.Derived_Type_Views is

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   function Create_Derived_Type
     (Index    : Gela.Semantic_Types.Type_View_Index;
      Parent   : not null Gela.Type_Categories.Type_View_Access;
      Decl     : Gela.Elements.Full_Type_Declarations
                   .Full_Type_Declaration_Access)
      return Gela.Type_Categories.Type_View_Access
   is
      Value : constant Type_View_Access := new Type_View'(Index, Parent, Decl);
   begin
      return Gela.Type_Categories.Type_View_Access (Value);
   end Create_Derived_Type;

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Type_View) return Gela.Type_Categories.Category_Kinds is
   begin
      return Self.Parent.Category;
   end Category;

   --------------------
   -- Component_Type --
   --------------------

   overriding function Component_Type
     (Self : Type_View) return Gela.Semantic_Types.Type_View_Index is
   begin
      return Gela.Types.Arrays.Array_Type_Access (Self.Parent).Component_Type;
   end Component_Type;

   ---------------
   -- Dimension --
   ---------------

   overriding function Dimension (Self : Type_View) return Positive is
   begin
      return Gela.Types.Arrays.Array_Type_Access (Self.Parent).Dimension;
   end Dimension;

   overriding function Defining_Name (Self : Type_View)
     return Gela.Elements.Defining_Names.Defining_Name_Access
   is
   begin
      return Gela.Elements.Defining_Names.Defining_Name_Access
        (Self.Decl.Names);
   end Defining_Name;

   ----------------------
   -- Get_Discriminant --
   ----------------------

   overriding function Get_Discriminant
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Gela.Types.Discriminated.Discriminated_Type_Access
        (Self.Parent).Get_Discriminant (Symbol);
   end Get_Discriminant;

   -------------------
   -- Get_Component --
   -------------------

   overriding function Get_Component
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access
   is
   begin
      return Gela.Types.Untagged_Records.Untagged_Record_Type_Access
        (Self.Parent).Get_Component (Symbol);
   end Get_Component;

   --------------------
   -- Get_Designated --
   --------------------

   overriding function Get_Designated
     (Self   : Type_View)
      return Gela.Elements.Subtype_Marks.Subtype_Mark_Access is
   begin
      return Gela.Types.Simple.Object_Access_Type_Access
        (Self.Parent).Get_Designated;
   end Get_Designated;

   -----------------
   -- Index_Types --
   -----------------

   overriding function Index_Types
     (Self : Type_View) return Gela.Types.Simple.Discrete_Type_Array is
   begin
      return Gela.Types.Arrays.Array_Type_Access (Self.Parent).Index_Types;
   end Index_Types;

   -----------------
   -- Index_Types --
   -----------------

   overriding function Index_Types
     (Self : Type_View) return Gela.Semantic_Types.Type_Index_Array is
   begin
      return Gela.Types.Arrays.Array_Type_Access (Self.Parent).Index_Types;
   end Index_Types;

   --------------
   -- Is_Array --
   --------------

   overriding function Is_Array (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Array;
   end Is_Array;

   ------------------
   -- Is_Character --
   ------------------

   overriding function Is_Character (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Character;
   end Is_Character;

   --------------------
   -- Is_Enumeration --
   --------------------

   overriding function Is_Enumeration (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Enumeration;
   end Is_Enumeration;

   ----------------------
   -- Is_Expected_Type --
   ----------------------

   overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Gela.Types.Type_View_Access)
      return Boolean is
   begin
      if Self.Is_The_Same_Type (Expected.all) then
         return True;
      end if;

      if Expected.Is_Universal then
         if Expected.Is_Integer then
            return Self.Category in Gela.Type_Categories.Any_Integer_Type;
         elsif Expected.Is_Real then
            return Self.Category in Gela.Type_Categories.Any_Real_Type;
         end if;
      end if;

      return False;
   end Is_Expected_Type;

   -----------------------
   -- Is_Floating_Point --
   -----------------------

   overriding function Is_Floating_Point (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Floating_Point;
   end Is_Floating_Point;

   ------------------------
   -- Is_Modular_Integer --
   ------------------------

   overriding function Is_Modular_Integer (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Modular_Integer;
   end Is_Modular_Integer;

   ----------------------
   -- Is_Object_Access --
   ----------------------

   overriding function Is_Object_Access (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Modular_Integer;
   end Is_Object_Access;

   ---------------
   -- Is_Record --
   ---------------

   overriding function Is_Record (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Record;
   end Is_Record;

   -------------
   -- Is_Root --
   -------------

   overriding function Is_Root (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Root;

   -----------------------
   -- Is_Signed_Integer --
   -----------------------

   overriding function Is_Signed_Integer (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Signed_Integer;
   end Is_Signed_Integer;

   ----------------------
   -- Is_The_Same_Type --
   ----------------------

   overriding function Is_The_Same_Type
     (Left  : Type_View;
      Right : Gela.Types.Type_View'Class) return Boolean
    is
      use type Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access;
   begin
      if Right in Type_View'Class and then
        Left.Decl = Type_View (Right).Decl
      then
         return True;
      end if;

      return False;
   end Is_The_Same_Type;

   ------------------
   -- Is_Universal --
   ------------------

   overriding function Is_Universal (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Universal;

   ---------------------
   -- Type_View_Index --
   ---------------------

   overriding function Type_View_Index
     (Self : Type_View) return Gela.Semantic_Types.Type_View_Index is
   begin
      return Self.Index;
   end Type_View_Index;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Type_View;
      Visiter : in out Gela.Types.Visitors.Type_Visitor'Class) is
   begin
      Self.Parent.Visit (Visiter);
   end Visit;

end Gela.Derived_Type_Views;
