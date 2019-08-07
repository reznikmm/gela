--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Lexical_Elements;
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

package Program.Nodes.Vectors is
   pragma Preelaborate;

   type Vector;

   function Create
    (Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
      return Vector;

   type Vector (<>) is new Program.Element_Vectors.Element_Vector
     and Program.Elements.Array_Component_Associations
       .Array_Component_Association_Vector
     and Program.Elements.Aspect_Specifications.Aspect_Specification_Vector
     and Program.Elements.Case_Expression_Paths.Case_Expression_Path_Vector
     and Program.Elements.Case_Paths.Case_Path_Vector
     and Program.Elements.Component_Clauses.Component_Clause_Vector
     and Program.Elements.Defining_Identifiers.Defining_Identifier_Vector
     and Program.Elements.Discrete_Ranges.Discrete_Range_Vector
     and Program.Elements.Discriminant_Associations
       .Discriminant_Association_Vector
     and Program.Elements.Discriminant_Specifications
       .Discriminant_Specification_Vector
     and Program.Elements.Elsif_Paths.Elsif_Path_Vector
     and Program.Elements.Enumeration_Literal_Specifications
       .Enumeration_Literal_Specification_Vector
     and Program.Elements.Exception_Handlers.Exception_Handler_Vector
     and Program.Elements.Expressions.Expression_Vector
     and Program.Elements.Formal_Package_Associations
       .Formal_Package_Association_Vector
     and Program.Elements.Identifiers.Identifier_Vector
     and Program.Elements.Parameter_Associations.Parameter_Association_Vector
     and Program.Elements.Parameter_Specifications
       .Parameter_Specification_Vector
     and Program.Elements.Record_Component_Associations
       .Record_Component_Association_Vector
     and Program.Elements.Select_Paths.Select_Path_Vector
     and Program.Elements.Variants.Variant_Vector
   with private;

private

   type Element_Array is array (Positive range <>)
     of Program.Elements.Element_Access;

   type Lexical_Element_Array is array (Positive range <>)
     of Program.Lexical_Elements.Lexical_Element_Access;

   type Vector (Elements, Tokens : Natural) is
     new Program.Element_Vectors.Element_Vector
     and Program.Elements.Array_Component_Associations
       .Array_Component_Association_Vector
     and Program.Elements.Aspect_Specifications.Aspect_Specification_Vector
     and Program.Elements.Case_Expression_Paths.Case_Expression_Path_Vector
     and Program.Elements.Case_Paths.Case_Path_Vector
     and Program.Elements.Component_Clauses.Component_Clause_Vector
     and Program.Elements.Defining_Identifiers.Defining_Identifier_Vector
     and Program.Elements.Discrete_Ranges.Discrete_Range_Vector
     and Program.Elements.Discriminant_Associations
       .Discriminant_Association_Vector
     and Program.Elements.Discriminant_Specifications
       .Discriminant_Specification_Vector
     and Program.Elements.Elsif_Paths.Elsif_Path_Vector
     and Program.Elements.Enumeration_Literal_Specifications
       .Enumeration_Literal_Specification_Vector
     and Program.Elements.Exception_Handlers.Exception_Handler_Vector
     and Program.Elements.Expressions.Expression_Vector
     and Program.Elements.Formal_Package_Associations
       .Formal_Package_Association_Vector
     and Program.Elements.Identifiers.Identifier_Vector
     and Program.Elements.Parameter_Associations.Parameter_Association_Vector
     and Program.Elements.Parameter_Specifications
       .Parameter_Specification_Vector
     and Program.Elements.Record_Component_Associations
       .Record_Component_Association_Vector
     and Program.Elements.Select_Paths.Select_Path_Vector
     and Program.Elements.Variants.Variant_Vector
   with record
      Element_List : Element_Array (1 .. Elements);
      Token_List   : Lexical_Element_Array (1 .. Tokens);
   end record;

   overriding function Get_Length (Self : Vector) return Positive;

   overriding function Element
     (Self  : Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access;

   overriding function Delimiter
     (Self  : Vector;
      Index : Positive)
     return Program.Lexical_Elements.Lexical_Element_Access;

end Program.Nodes.Vectors;
