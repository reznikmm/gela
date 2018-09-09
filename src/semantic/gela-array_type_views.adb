with Gela.Compilations;
with Gela.Type_Managers;
with Gela.Elements.Defining_Identifiers;
pragma Unreferenced (Gela.Elements.Defining_Identifiers);

package body Gela.Array_Type_Views is

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Type_View) return Gela.Type_Categories.Category_Kinds is
   begin
      return Self.Category;
   end Category;

   --------------------
   -- Component_Type --
   --------------------

   overriding function Component_Type
     (Self : Type_View) return Gela.Semantic_Types.Type_View_Index is
   begin
      return Self.Component;
   end Component_Type;

   ----------------------
   -- Create_Full_Type --
   ----------------------

   function Create_Full_Type
     (Index     : Gela.Semantic_Types.Type_View_Index;
      Category  : Gela.Type_Categories.Category_Kinds;
      Decl      : Gela.Elements.Full_Type_Declarations
                    .Full_Type_Declaration_Access;
      Component : Gela.Semantic_Types.Type_View_Index;
      Indexes   : Gela.Semantic_Types.Type_Index_Array)
      return Gela.Type_Categories.Type_View_Access
   is
      Value : constant Type_View_Access :=
        new Type_View'(Index     => Index,
                       Category  => Category, Decl => Decl,
                       Component => Component,
                       Length    => Indexes'Length, Indexes => Indexes);
   begin
      return Gela.Type_Categories.Type_View_Access (Value);
   end Create_Full_Type;

   -------------------
   -- Defining_Name --
   -------------------

   overriding function Defining_Name (Self : Type_View)
     return Gela.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Gela.Elements.Defining_Names.Defining_Name_Access
        (Self.Decl.Names);
   end Defining_Name;

   ---------------
   -- Dimension --
   ---------------

   overriding function Dimension (Self : Type_View) return Positive is
   begin
      return Self.Length;
   end Dimension;

   -----------------
   -- Index_Types --
   -----------------

   overriding function Index_Types
     (Self : Type_View)
      return Gela.Types.Simple.Discrete_Type_Array
   is
      X : constant Gela.Compilations.Compilation_Access :=
        Self.Decl.Enclosing_Compilation;
      TM : constant Gela.Type_Managers.Type_Manager_Access := X.Context.Types;
      Result : Gela.Types.Simple.Discrete_Type_Array (1 .. Self.Length);
   begin
      for J in Result'Range loop
         Result (J) :=
           Gela.Types.Simple.Discrete_Type_Access (TM.Get (Self.Indexes (J)));
      end loop;

      return Result;
   end Index_Types;

   -----------------
   -- Index_Types --
   -----------------

   overriding function Index_Types
     (Self : Type_View) return Gela.Semantic_Types.Type_Index_Array is
   begin
      return Self.Indexes;
   end Index_Types;
   --------------
   -- Is_Array --
   --------------

   overriding function Is_Array (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Array;

   ------------------
   -- Is_Character --
   ------------------

   overriding function Is_Character (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Character;

   --------------------
   -- Is_Enumeration --
   --------------------

   overriding function Is_Enumeration (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Enumeration;

   ----------------------
   -- Is_Expected_Type --
   ----------------------

   overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Gela.Types.Type_View_Access)
      return Boolean is
   begin
      return Self.Is_The_Same_Type (Expected.all);
   end Is_Expected_Type;

   -----------------------
   -- Is_Floating_Point --
   -----------------------

   overriding function Is_Floating_Point (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Floating_Point;

   ------------------------
   -- Is_Modular_Integer --
   ------------------------

   overriding function Is_Modular_Integer (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Modular_Integer;

   ----------------------
   -- Is_Object_Access --
   ----------------------

   overriding function Is_Object_Access (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Object_Access;

   ---------------
   -- Is_Record --
   ---------------

   overriding function Is_Record (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
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
      pragma Unreferenced (Self);
   begin
      return False;
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
        Left.Decl = Type_View'Class (Right).Decl
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
      Visiter.Array_Type
        (Gela.Types.Arrays.Array_Type_Access (Self));
   end Visit;

end Gela.Array_Type_Views;
