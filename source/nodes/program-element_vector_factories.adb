--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Array_Component_Association_Vectors;
with Program.Nodes.Aspect_Specification_Vectors;
with Program.Nodes.Element_Vectors;
with Program.Nodes.Expression_Vectors;
with Program.Nodes.Case_Expression_Path_Vectors;
with Program.Nodes.Case_Path_Vectors;
with Program.Nodes.Component_Clause_Vectors;
with Program.Nodes.Defining_Identifier_Vectors;
with Program.Nodes.Discrete_Range_Vectors;
with Program.Nodes.Discriminant_Association_Vectors;
with Program.Nodes.Discriminant_Specification_Vectors;
with Program.Nodes.Elsif_Path_Vectors;
with Program.Nodes.Enumeration_Literal_Specification_Vectors;
with Program.Nodes.Exception_Handler_Vectors;
with Program.Nodes.Formal_Package_Association_Vectors;
with Program.Nodes.Identifier_Vectors;
with Program.Nodes.Parameter_Association_Vectors;
with Program.Nodes.Parameter_Specification_Vectors;
with Program.Nodes.Record_Component_Association_Vectors;
with Program.Nodes.Select_Path_Vectors;
with Program.Nodes.Variant_Vectors;
with Program.Storage_Pools;

package body Program.Element_Vector_Factories is

   type Array_Component_Association_Vector_Access is
     not null access Program.Nodes.Array_Component_Association_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Aspect_Specification_Vector_Access is
     not null access Program.Nodes.Aspect_Specification_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Case_Expression_Path_Vector_Access is
     not null access Program.Nodes.Case_Expression_Path_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Case_Path_Vector_Access is
     not null access Program.Nodes.Case_Path_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Component_Clause_Vector_Access is
     not null access Program.Nodes.Component_Clause_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Defining_Identifier_Vector_Access is
     not null access Program.Nodes.Defining_Identifier_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Discrete_Range_Vector_Access is
     not null access Program.Nodes.Discrete_Range_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Discriminant_Association_Vector_Access is
     not null access Program.Nodes.Discriminant_Association_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Discriminant_Specification_Vector_Access is
     not null access Program.Nodes.Discriminant_Specification_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Element_Vector_Access is
     not null access Program.Nodes.Element_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Elsif_Path_Vector_Access is
     not null access Program.Nodes.Elsif_Path_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Enumeration_Literal_Specification_Vector_Access is not null access
     Program.Nodes.Enumeration_Literal_Specification_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Exception_Handler_Vector_Access is
     not null access Program.Nodes.Exception_Handler_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Expression_Vector_Access is
     not null access Program.Nodes.Expression_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Package_Association_Vector_Access is not null access
     Program.Nodes.Formal_Package_Association_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Identifier_Vector_Access is not null access
     Program.Nodes.Identifier_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Parameter_Association_Vector_Access is not null access
     Program.Nodes.Parameter_Association_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Parameter_Specification_Vector_Access is not null access
     Program.Nodes.Parameter_Specification_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Record_Component_Association_Vector_Access is not null access
     Program.Nodes.Record_Component_Association_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Select_Path_Vector_Access is not null access
     Program.Nodes.Select_Path_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   type Variant_Vector_Access is not null access
     Program.Nodes.Variant_Vectors.Vector
       with Storage_Pool => Program.Storage_Pools.Pool;

   -----------------------------------------------
   -- Create_Array_Component_Association_Vector --
   -----------------------------------------------

   not overriding function Create_Array_Component_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Array_Component_Associations
                        .Array_Component_Association_Vector_Access
   is
      Result : constant Array_Component_Association_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Array_Component_Association_Vectors.Vector'
            (Program.Nodes.Array_Component_Association_Vectors.Create (Each));
   begin
      return Program.Elements.Array_Component_Associations
               .Array_Component_Association_Vector_Access (Result);
   end Create_Array_Component_Association_Vector;

   ----------------------------------------
   -- Create_Aspect_Specification_Vector --
   ----------------------------------------

   not overriding function Create_Aspect_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Aspect_Specifications
                        .Aspect_Specification_Vector_Access
   is
      Result : constant Aspect_Specification_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Aspect_Specification_Vectors.Vector'
            (Program.Nodes.Aspect_Specification_Vectors.Create (Each));
   begin
      return Program.Elements.Aspect_Specifications
               .Aspect_Specification_Vector_Access (Result);
   end Create_Aspect_Specification_Vector;

   ----------------------------------------
   -- Create_Case_Expression_Path_Vector --
   ----------------------------------------

   not overriding function Create_Case_Expression_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Case_Expression_Paths
                        .Case_Expression_Path_Vector_Access
   is
      Result : constant Case_Expression_Path_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Case_Expression_Path_Vectors.Vector'
            (Program.Nodes.Case_Expression_Path_Vectors.Create (Each));
   begin
      return Program.Elements.Case_Expression_Paths
               .Case_Expression_Path_Vector_Access (Result);
   end Create_Case_Expression_Path_Vector;

   -----------------------------
   -- Create_Case_Path_Vector --
   -----------------------------

   not overriding function Create_Case_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Case_Paths
                        .Case_Path_Vector_Access
   is
      Result : constant Case_Path_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Case_Path_Vectors.Vector'
            (Program.Nodes.Case_Path_Vectors.Create (Each));
   begin
      return Program.Elements.Case_Paths .Case_Path_Vector_Access (Result);
   end Create_Case_Path_Vector;

   ------------------------------------
   -- Create_Component_Clause_Vector --
   ------------------------------------

   not overriding function Create_Component_Clause_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Component_Clauses
                        .Component_Clause_Vector_Access
   is
      Result : constant Component_Clause_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Component_Clause_Vectors.Vector'
            (Program.Nodes.Component_Clause_Vectors.Create (Each));
   begin
      return Program.Elements.Component_Clauses
               .Component_Clause_Vector_Access (Result);
   end Create_Component_Clause_Vector;

   ---------------------------------------
   -- Create_Defining_Identifier_Vector --
   ---------------------------------------

   not overriding function Create_Defining_Identifier_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Defining_Identifiers
                        .Defining_Identifier_Vector_Access
   is
      Result : constant Defining_Identifier_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Defining_Identifier_Vectors.Vector'
            (Program.Nodes.Defining_Identifier_Vectors.Create (Each));
   begin
      return Program.Elements.Defining_Identifiers
               .Defining_Identifier_Vector_Access (Result);
   end Create_Defining_Identifier_Vector;

   ----------------------------------
   -- Create_Discrete_Range_Vector --
   ----------------------------------

   not overriding function Create_Discrete_Range_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Discrete_Ranges
                        .Discrete_Range_Vector_Access
   is
      Result : constant Discrete_Range_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Discrete_Range_Vectors.Vector'
            (Program.Nodes.Discrete_Range_Vectors.Create (Each));
   begin
      return Program.Elements.Discrete_Ranges
               .Discrete_Range_Vector_Access (Result);
   end Create_Discrete_Range_Vector;

   --------------------------------------------
   -- Create_Discriminant_Association_Vector --
   --------------------------------------------

   not overriding function Create_Discriminant_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Discriminant_Associations
                        .Discriminant_Association_Vector_Access
   is
      Result : constant Discriminant_Association_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Discriminant_Association_Vectors.Vector'
            (Program.Nodes.Discriminant_Association_Vectors.Create (Each));
   begin
      return Program.Elements.Discriminant_Associations
               .Discriminant_Association_Vector_Access (Result);
   end Create_Discriminant_Association_Vector;

   ----------------------------------------------
   -- Create_Discriminant_Specification_Vector --
   ----------------------------------------------

   not overriding function Create_Discriminant_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Elements.Discriminant_Specifications
                        .Discriminant_Specification_Vector_Access
   is
      Result : constant Discriminant_Specification_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Discriminant_Specification_Vectors.Vector'
            (Program.Nodes.Discriminant_Specification_Vectors.Create (Each));
   begin
      return Program.Elements.Discriminant_Specifications
               .Discriminant_Specification_Vector_Access (Result);
   end Create_Discriminant_Specification_Vector;

   ---------------------------
   -- Create_Element_Vector --
   ---------------------------

   not overriding function Create_Element_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return not null Program.Element_Vectors.Element_Vector_Access
   is
      Result : constant Element_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Element_Vectors.Vector'
          (Program.Nodes.Element_Vectors.Create (Each));
   begin
      return Program.Element_Vectors.Element_Vector_Access (Result);
   end Create_Element_Vector;

   ------------------------------
   -- Create_Elsif_Path_Vector --
   ------------------------------

   not overriding function Create_Elsif_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access
   is
      Result : constant Elsif_Path_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Elsif_Path_Vectors.Vector'
          (Program.Nodes.Elsif_Path_Vectors.Create (Each));
   begin
      return Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access (Result);
   end Create_Elsif_Path_Vector;

   -----------------------------------------------------
   -- Create_Enumeration_Literal_Specification_Vector --
   -----------------------------------------------------

   not overriding function Create_Enumeration_Literal_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Enumeration_Literal_Specifications
                         .Enumeration_Literal_Specification_Vector_Access
   is
      Result : constant Enumeration_Literal_Specification_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Enumeration_Literal_Specification_Vectors.Vector'
          (Program.Nodes.Enumeration_Literal_Specification_Vectors.Create
             (Each));
   begin
      return Program.Elements.Enumeration_Literal_Specifications
                .Enumeration_Literal_Specification_Vector_Access (Result);
   end Create_Enumeration_Literal_Specification_Vector;

   -------------------------------------
   -- Create_Exception_Handler_Vector --
   -------------------------------------

   not overriding function Create_Exception_Handler_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Exception_Handlers
                         .Exception_Handler_Vector_Access
   is
      Result : constant Exception_Handler_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Exception_Handler_Vectors.Vector'
          (Program.Nodes.Exception_Handler_Vectors.Create (Each));
   begin
      return Program.Elements.Exception_Handlers
               .Exception_Handler_Vector_Access (Result);
   end Create_Exception_Handler_Vector;

   ------------------------------
   -- Create_Expression_Vector --
   ------------------------------

   not overriding function Create_Expression_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Expressions.Expression_Vector_Access
   is
      Result : constant Expression_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Expression_Vectors.Vector'
          (Program.Nodes.Expression_Vectors.Create (Each));
   begin
      return Program.Elements.Expressions.Expression_Vector_Access (Result);
   end Create_Expression_Vector;

   ----------------------------------------------
   -- Create_Formal_Package_Association_Vector --
   ----------------------------------------------

   not overriding function Create_Formal_Package_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Formal_Package_Associations
                         .Formal_Package_Association_Vector_Access
   is
      Result : constant Formal_Package_Association_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Formal_Package_Association_Vectors.Vector'
          (Program.Nodes.Formal_Package_Association_Vectors.Create
             (Each));
   begin
      return Program.Elements.Formal_Package_Associations
                .Formal_Package_Association_Vector_Access (Result);
   end Create_Formal_Package_Association_Vector;

   ------------------------------
   -- Create_Identifier_Vector --
   ------------------------------

   not overriding function Create_Identifier_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Identifiers
                         .Identifier_Vector_Access
   is
      Result : constant Identifier_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Identifier_Vectors.Vector'
          (Program.Nodes.Identifier_Vectors.Create (Each));
   begin
      return Program.Elements.Identifiers.Identifier_Vector_Access (Result);
   end Create_Identifier_Vector;

   -----------------------------------------
   -- Create_Parameter_Association_Vector --
   -----------------------------------------

   not overriding function Create_Parameter_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Parameter_Associations
                         .Parameter_Association_Vector_Access
   is
      Result : constant Parameter_Association_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Parameter_Association_Vectors.Vector'
          (Program.Nodes.Parameter_Association_Vectors.Create
             (Each));
   begin
      return Program.Elements.Parameter_Associations
                .Parameter_Association_Vector_Access (Result);
   end Create_Parameter_Association_Vector;

   -------------------------------------------
   -- Create_Parameter_Specification_Vector --
   -------------------------------------------

   not overriding function Create_Parameter_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Parameter_Specifications
                         .Parameter_Specification_Vector_Access
   is
      Result : constant Parameter_Specification_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Parameter_Specification_Vectors.Vector'
          (Program.Nodes.Parameter_Specification_Vectors.Create
             (Each));
   begin
      return Program.Elements.Parameter_Specifications
                .Parameter_Specification_Vector_Access (Result);
   end Create_Parameter_Specification_Vector;

   ------------------------------------------------
   -- Create_Record_Component_Association_Vector --
   ------------------------------------------------

   not overriding function Create_Record_Component_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Record_Component_Associations
                         .Record_Component_Association_Vector_Access
   is
      Result : constant Record_Component_Association_Vector_Access :=
        new (Self.Subpool)
          Program.Nodes.Record_Component_Association_Vectors.Vector'
          (Program.Nodes.Record_Component_Association_Vectors.Create
             (Each));
   begin
      return Program.Elements.Record_Component_Associations
                .Record_Component_Association_Vector_Access (Result);
   end Create_Record_Component_Association_Vector;

   -------------------------------
   -- Create_Select_Path_Vector --
   -------------------------------

   not overriding function Create_Select_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Select_Paths.Select_Path_Vector_Access
   is
      Result : constant Select_Path_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Select_Path_Vectors.Vector'
          (Program.Nodes.Select_Path_Vectors.Create (Each));
   begin
      return Program.Elements.Select_Paths.Select_Path_Vector_Access (Result);
   end Create_Select_Path_Vector;

   ---------------------------
   -- Create_Variant_Vector --
   ---------------------------

   not overriding function Create_Variant_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return not null Program.Elements.Variants.Variant_Vector_Access
   is
      Result : constant Variant_Vector_Access :=
        new (Self.Subpool) Program.Nodes.Variant_Vectors.Vector'
          (Program.Nodes.Variant_Vectors.Create (Each));
   begin
      return Program.Elements.Variants.Variant_Vector_Access (Result);
   end Create_Variant_Vector;

end Program.Element_Vector_Factories;
