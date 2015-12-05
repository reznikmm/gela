--  This package provides Compilation_Unit visiter interface

with Gela.Elements.Pragmas;
with Gela.Declarations.Package_Declarations;

package Gela.Element_Visiters is
   pragma Preelaborate;

   type Visiter is limited interface;

   not overriding procedure Pragma_Element
     (Self : access Visiter;
      Unit : aliased Gela.Elements.Pragmas.Pragma_Element'Class) is abstract;

   not overriding procedure Package_Declaration
     (Self : access Visiter;
      Unit : aliased Gela.Declarations.Package_Declarations
                       .Package_Declaration'Class) is abstract;

end Gela.Element_Visiters;
