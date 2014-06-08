with Gela.Int.Defining_Names;
with Gela.Int.Expressions;

package Gela.Int.Visiters is
   pragma Preelaborate;

   type Visiter is limited interface;

   not overriding procedure Defining_Name
     (Self  : access Visiter;
      Value : Gela.Int.Defining_Names.Defining_Name) is abstract;

   not overriding procedure Expression
     (Self  : access Visiter;
      Value : Gela.Int.Expressions.Expression) is abstract;

end Gela.Int.Visiters;
