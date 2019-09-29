--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Pools.Subpools;

with Program.Element_Vectors;
with Program.Elements.Expressions;
with Program.Elements.Array_Component_Associations;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Case_Expression_Paths;
with Program.Elements.Case_Paths;
with Program.Elements.Component_Clauses;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Discriminant_Associations;
with Program.Elements.Discriminant_Specifications;
with Program.Elements.Elsif_Paths;
with Program.Elements.Enumeration_Literal_Specifications;
with Program.Elements.Exception_Handlers;
with Program.Elements.Formal_Package_Associations;
with Program.Elements.Identifiers;
with Program.Elements.Parameter_Associations;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Record_Component_Associations;
with Program.Elements.Select_Paths;
with Program.Elements.Variants;

package Program.Element_Vector_Factories is
   type Element_Vector_Factory
     (Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle) is
       tagged limited private;

   not overriding function Create_Element_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return Program.Element_Vectors.Element_Vector_Access;

   not overriding function Create_Expression_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return Program.Elements.Expressions.Expression_Vector_Access;

   not overriding function Create_Array_Component_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Array_Component_Associations
                        .Array_Component_Association_Vector_Access;

   not overriding function Create_Aspect_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Aspect_Specifications
                        .Aspect_Specification_Vector_Access;

   not overriding function Create_Case_Expression_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Case_Expression_Paths
                        .Case_Expression_Path_Vector_Access;

   not overriding function Create_Case_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Case_Paths.Case_Path_Vector_Access;

   not overriding function Create_Component_Clause_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Component_Clauses
                        .Component_Clause_Vector_Access;

   not overriding function Create_Defining_Identifier_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Defining_Identifiers
                        .Defining_Identifier_Vector_Access;

   not overriding function Create_Discrete_Range_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Discrete_Ranges
                        .Discrete_Range_Vector_Access;

   not overriding function Create_Discriminant_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Discriminant_Associations
                        .Discriminant_Association_Vector_Access;

   not overriding function Create_Discriminant_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Discriminant_Specifications
                        .Discriminant_Specification_Vector_Access;

   not overriding function Create_Elsif_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access;

   not overriding function Create_Enumeration_Literal_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Enumeration_Literal_Specifications
                        .Enumeration_Literal_Specification_Vector_Access;

   not overriding function Create_Exception_Handler_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Exception_Handlers
                        .Exception_Handler_Vector_Access;

   not overriding function Create_Formal_Package_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Formal_Package_Associations
                        .Formal_Package_Association_Vector_Access;

   not overriding function Create_Identifier_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Identifiers.Identifier_Vector_Access;

   not overriding function Create_Parameter_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Parameter_Associations
                        .Parameter_Association_Vector_Access;

   not overriding function Create_Parameter_Specification_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Parameter_Specifications
                        .Parameter_Specification_Vector_Access;

   not overriding function Create_Record_Component_Association_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Record_Component_Associations
                        .Record_Component_Association_Vector_Access;

   not overriding function Create_Select_Path_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Select_Paths
                        .Select_Path_Vector_Access;

   not overriding function Create_Variant_Vector
    (Self : Element_Vector_Factory;
     Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
     return Program.Elements.Variants
                        .Variant_Vector_Access;

private
   type Element_Vector_Factory
     (Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle) is
       tagged limited null record;

end Program.Element_Vector_Factories;
