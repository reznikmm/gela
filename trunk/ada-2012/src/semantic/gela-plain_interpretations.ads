--  This package provides trivial Interpretation_Manager

with Ada.Containers.Vectors;

with Gela.Elements.Defining_Names;
with Gela.Contexts;
with Gela.Interpretations;
with Gela.Lexical_Types;
with Gela.Semantic_Types;
with Gela.Plian_Int_Sets;
with Gela.Int_Sets;

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
      Plian_Int_Set : Gela.Plian_Int_Sets.Interpretation_Set_Access :=
           new Gela.Plian_Int_Sets.Interpretation_Set
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
      Tipe   : Gela.Semantic_Types.Type_Index;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding procedure Add_Attr_Function
     (Self   : in out Interpretation_Manager;
      Kind   : Gela.Lexical_Types.Predefined_Symbols.Attribute;
      Down   : Gela.Interpretations.Interpretation_Index_Array;
      Result : in out Gela.Interpretations.Interpretation_Set_Index);

   overriding function Get_Cursor
     (Self   : in out Interpretation_Manager;
      Set    : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Cursor'Class;
   --  Get cursor to iterate over all interpretations in Set

   overriding procedure Visit
     (Self   : in out Interpretation_Manager;
      Index  : Gela.Interpretations.Interpretation_Index;
      Target : in out Gela.Interpretations.Visiter'Class);
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

end Gela.Plain_Interpretations;
