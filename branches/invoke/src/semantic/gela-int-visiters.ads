with Gela.Int.Symbol_Interpretations;

package Gela.Int.Visiters is
   pragma Preelaborate;

   type Visiter is limited interface;

   not overriding procedure Symbol_Interpretation
     (Self  : access Visiter;
      Value : Gela.Int.Symbol_Interpretations.Symbol_Interpretation) is null;

   type Solution_Visiter is limited interface;

end Gela.Int.Visiters;
