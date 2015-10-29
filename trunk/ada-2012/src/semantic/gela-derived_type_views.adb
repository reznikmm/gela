package body Gela.Derived_Type_Views is

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   function Create_Derived_Type
     (Parent   : not null Gela.Type_Categories.Type_View_Access;
      Decl     : Gela.Elements.Full_Type_Declarations
                   .Full_Type_Declaration_Access)
      return Gela.Type_Categories.Type_View_Access
   is
      Value : constant Type_View_Access := new Type_View'(Parent, Decl);
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

   ----------------------
   -- Get_Discriminant --
   ----------------------

   overriding function Get_Discriminant
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Parent.Get_Discriminant (Symbol);
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
      return Self.Parent.Get_Component (Symbol);
   end Get_Component;

   --------------------
   -- Get_Designated --
   --------------------

   overriding function Get_Designated
     (Self   : Type_View)
      return Gela.Elements.Subtype_Indications.Subtype_Indication_Access
   is
   begin
      return Self.Parent.Get_Designated;
   end Get_Designated;

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

   ----------------------
   -- Is_Expected_Type --
   ----------------------

   overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Gela.Types.Type_View_Access)
      return Boolean
   is
      use type Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access;

   begin
      if Expected.all in Type_View and then
        Self.Decl = Type_View (Expected.all).Decl
      then
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

   -----------------------
   -- Is_Signed_Integer --
   -----------------------

   overriding function Is_Signed_Integer (Self : Type_View) return Boolean is
   begin
      return Self.Parent.Is_Signed_Integer;
   end Is_Signed_Integer;

   ------------------
   -- Is_Universal --
   ------------------

   overriding function Is_Universal (Self : Type_View) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Universal;

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
