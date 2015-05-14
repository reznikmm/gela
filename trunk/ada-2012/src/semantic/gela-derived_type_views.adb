package body Gela.Derived_Type_Views is

   -------------------------
   -- Create_Derived_Type --
   -------------------------

   function Create_Derived_Type
     (Parent   : not null Gela.Type_Views.Type_View_Access;
      Decl     : Gela.Elements.Full_Type_Declarations
                   .Full_Type_Declaration_Access)
      return Gela.Type_Views.Type_View_Access
   is
      Value : constant Type_View_Access := new Type_View'(Parent, Decl);
   begin
      return Gela.Type_Views.Type_View_Access (Value);
   end Create_Derived_Type;

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Type_View) return Gela.Type_Views.Category_Kinds is
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

   ----------------------
   -- Is_Expected_Type --
   ----------------------

   overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Gela.Type_Views.Type_View_Access)
      return Boolean
   is
      use type Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access;

      Expected_Category : constant Gela.Type_Views.Category_Kinds :=
        Expected.Category;

   begin
      if Expected.all in Type_View and then
        Self.Decl = Type_View (Expected.all).Decl
      then
         return True;
      end if;

      case Expected_Category is
         when Gela.Type_Views.An_Universal_Integer =>
            return Self.Category in Gela.Type_Views.Any_Integer_Type;
         when Gela.Type_Views.An_Universal_Real =>
            return Self.Category in Gela.Type_Views.Any_Real_Type;
         when others =>
            null;
      end case;

      return False;
   end Is_Expected_Type;

end Gela.Derived_Type_Views;