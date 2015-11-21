--  This package provides Compilation_Unit visiter interface

package Gela.Compilation_Units.Visiters is
   pragma Preelaborate;

   type Visiter is limited interface;

   not overriding procedure Library_Unit_Declaration
     (Self : access Visiter;
      Unit : not null Gela.Compilation_Units.Library_Unit_Declaration_Access)
        is abstract;

   not overriding procedure Library_Unit_Body
     (Self : access Visiter;
      Unit : not null Gela.Compilation_Units.Library_Unit_Body_Access)
        is abstract;

   not overriding procedure Subunit
     (Self : access Visiter;
      Unit : not null Gela.Compilation_Units.Subunit_Access)
        is abstract;

end Gela.Compilation_Units.Visiters;
