with Gela.Int.Attr_Functions;
with Gela.Int.Defining_Names;
with Gela.Int.Expressions;
with Gela.Int.Placeholders;
with Gela.Int.Tuples;

package Gela.Int.Visiters is
   pragma Preelaborate;

   type Visiter is limited interface;

   not overriding procedure Attr_Function
     (Self  : access Visiter;
      Value : Gela.Int.Attr_Functions.Attr_Function) is abstract;

   not overriding procedure Chosen_Tuple
     (Self  : access Visiter;
      Value : Gela.Int.Tuples.Chosen_Tuple) is abstract;

   not overriding procedure Defining_Name
     (Self  : access Visiter;
      Value : Gela.Int.Defining_Names.Defining_Name) is abstract;

   not overriding procedure Expression
     (Self  : access Visiter;
      Value : Gela.Int.Expressions.Expression) is abstract;

   not overriding procedure Placeholder
     (Self  : access Visiter;
      Value : Gela.Int.Placeholders.Placeholder) is abstract;

   not overriding procedure Tuple
     (Self  : access Visiter;
      Value : Gela.Int.Tuples.Tuple) is abstract;

end Gela.Int.Visiters;
