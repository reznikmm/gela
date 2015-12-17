with Gela.Semantic_Types;
with Gela.Types.Simple;

package Gela.Types.Arrays is
   pragma Preelaborate;

   type Array_Type is limited interface and Type_View;
   type Array_Type_Access is access all Array_Type'Class;
   for Array_Type_Access'Storage_Size use 0;

   not overriding function Index_Types
     (Self : Array_Type) return Gela.Types.Simple.Discrete_Type_Array
       is abstract;

   not overriding function Index_Types
     (Self : Array_Type) return Gela.Semantic_Types.Type_Index_Array
       is abstract;

   not overriding function Component_Type
     (Self : Array_Type) return Gela.Semantic_Types.Type_Index
       is abstract;

   not overriding function Dimension (Self : Array_Type) return Positive
     is abstract;

end Gela.Types.Arrays;
