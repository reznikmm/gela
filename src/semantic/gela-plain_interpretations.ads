--  This package provides trivial Interpretation_Manager

with Ada.Containers.Vectors;

with Gela.Contexts;
with Gela.Elements.Defining_Names;
with Gela.Int_Sets;
with Gela.Interpretations;
with Gela.Lexical_Types;
with Gela.Plain_Int_Sets;
with Gela.Semantic_Types;

package Gela.Plain_Interpretations is
   pragma Preelaborate;

   type Interpretation_Manager (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Interpretations.Interpretation_Manager
       and Gela.Int_Sets.Index_Provider with private;

   type Interpretation_Manager_Access is
     access all Interpretation_Manager'Class;

private

   use type Gela.Interpretations.Interpretation_Index;
   use type Gela.Interpretations.Interpretation_Set_Index;

   Batch_Size : constant := 16 * 1024;

   subtype Set_Batch is Gela.Interpretations.Interpretation_Set_Index
     range 0 .. Interpretations.Interpretation_Set_Index'Last / Batch_Size;

   subtype Item_Index is Gela.Interpretations.Interpretation_Index
     range 0 .. Gela.Interpretations.Interpretation_Index'Last / Batch_Size;

   package Set_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Set_Batch,
      Element_Type => Gela.Int_Sets.Interpretation_Set_Access,
      "="          => Gela.Int_Sets."=");

   package Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Item_Index,
      Element_Type => Gela.Int_Sets.Interpretation_Set_Access,
      "="          => Gela.Int_Sets."=");

   type Interpretation_Manager (Context : Gela.Contexts.Context_Access) is
     limited new Gela.Interpretations.Interpretation_Manager
       and Gela.Int_Sets.Index_Provider with
   record
      Plain_Int_Set : Gela.Plain_Int_Sets.Interpretation_Set_Access :=
           new Gela.Plain_Int_Sets.Interpretation_Set
                 (Interpretation_Manager'Unchecked_Access);
      Set_Batches  : Set_Vectors.Vector;
      Item_Batches : Item_Vectors.Vector;
   end record;

   overriding procedure Add_Defining_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Expression
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Kind   : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds :=
        Gela.Interpretations.Unknown;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Expression_Category
     (Self   : in out Interpretation_Manager;
      Match  : not null Gela.Interpretations.Type_Matcher_Access;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Attr_Function
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Placeholder
     (Self   : in out Interpretation_Manager;
      Kind   : Gela.Interpretations.Placeholder_Kind;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Symbol
     (Self   : in out Interpretation_Manager;
      Symbol : Gela.Lexical_Types.Symbol;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Tuple
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Set_Index;
      Right  : Gela.Interpretations.Interpretation_Tuple_Index;
      Result : out Gela.Interpretations.Interpretation_Tuple_Index);

   overriding procedure Add_Tuple_List
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Tuple_Index;
      Right  : Gela.Interpretations.Interpretation_Tuple_List_Index;
      Result : out Gela.Interpretations.Interpretation_Tuple_List_Index);

   overriding procedure Get_Defining_Name_Index
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Result : out Gela.Interpretations.Interpretation_Index);

   overriding procedure Get_Expression_Index
     (Self   : in out Interpretation_Manager;
      Tipe   : Gela.Semantic_Types.Type_View_Index;
      Result : out Gela.Interpretations.Interpretation_Index);

   overriding procedure Get_Tuple_Index
     (Self   : in out Interpretation_Manager;
      Left   : Gela.Interpretations.Interpretation_Index;
      Right  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Interpretations.Interpretation_Index);

   overriding function Get_Tuple
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Tuple_Index)
      return Gela.Interpretations.Interpretation_Set_Index_Array;

   overriding function Get_Tuple_List
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Tuple_List_Index)
      return Gela.Interpretations.Interpretation_Tuple_Index_Array;

   overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Index;
      Target : in out Gela.Interpretations.Down_Visiter'Class);
   --  For given interpretations call Target visiter

   overriding procedure Get_Down_Interpretation
     (Self     : in out Interpretation_Manager;
      Value    : Gela.Interpretations.Interpretation_Index;
      Index    : Positive;
      Result   : out Gela.Interpretations.Interpretation_Index);
   --  Return interpretation from which Value was derived

   overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access);

   overriding procedure Reserve_Indexes
     (Self : in out Interpretation_Manager;
      Set  : Gela.Int_Sets.Interpretation_Set_Access;
      From : out Gela.Interpretations.Interpretation_Set_Index;
      To   : out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Reserve_Indexes
     (Self : in out Interpretation_Manager;
      Set  : Gela.Int_Sets.Interpretation_Set_Access;
      From : out Gela.Interpretations.Interpretation_Index;
      To   : out Gela.Interpretations.Interpretation_Index);

   overriding function Symbols
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Symbol_Iterators
                 .Forward_Iterator'Class;

   overriding function Defining_Names
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Defining_Name_Iterators
                 .Forward_Iterator'Class;

   overriding function Expressions
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Expression_Iterators
                 .Forward_Iterator'Class;

   overriding function Categories
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Category_Iterators
                 .Forward_Iterator'Class;

   overriding function Profiles
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Profile_Iterators
                 .Forward_Iterator'Class;

   overriding function Each
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
        return Gela.Interpretations.Any_Iterators
                 .Forward_Iterator'Class;

end Gela.Plain_Interpretations;
