--  This package provides Compilation_Unit visiter interface

with Gela.Elements.Pragmas;
with Gela.Elements.Defining_Names;
with Gela.Declarations.Entry_Body_Declarations;
with Gela.Declarations.Entry_Declarations;
with Gela.Declarations.Generic_Subprogram_Declarations;
with Gela.Declarations.Package_Declarations;
with Gela.Declarations.Subprogram_Body_Declarations;
with Gela.Declarations.Subprogram_Declarations;
with Gela.Declarations.Subtype_Declarations;
with Gela.Declarations.Type_Declarations;

package Gela.Element_Visiters is
   pragma Preelaborate;

   type Abstract_Visiter is limited interface;

   not overriding procedure Pragma_Element
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Elements.Pragmas.Pragma_Element'Class)
       is abstract;

   not overriding procedure Defining_Name
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Elements.Defining_Names.Defining_Name'Class)
       is abstract;

   not overriding procedure Entry_Body_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Entry_Body_Declarations
                       .Entry_Body_Declaration'Class) is abstract;

   not overriding procedure Entry_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Entry_Declarations
                       .Entry_Declaration'Class) is abstract;

   not overriding procedure Generic_Subprogram_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Generic_Subprogram_Declarations
                       .Generic_Subprogram_Declaration'Class) is abstract;

   not overriding procedure Package_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Package_Declarations
                       .Package_Declaration'Class) is abstract;

   not overriding procedure Subprogram_Body_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Subprogram_Body_Declarations
                       .Subprogram_Body_Declaration'Class) is abstract;

   not overriding procedure Subprogram_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Subprogram_Declarations
                       .Subprogram_Declaration'Class) is abstract;

   not overriding procedure Subtype_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Subtype_Declarations
                       .Subtype_Declaration'Class) is abstract;

   not overriding procedure Type_Declaration
     (Self    : access Abstract_Visiter;
      Element : aliased Gela.Declarations.Type_Declarations
                       .Type_Declaration'Class) is abstract;

   type Visiter is limited interface and Abstract_Visiter;

   overriding procedure Pragma_Element
     (Self    : access Visiter;
      Element : aliased Gela.Elements.Pragmas.Pragma_Element'Class) is null;

   overriding procedure Defining_Name
     (Self    : access Visiter;
      Element : aliased Gela.Elements.Defining_Names.Defining_Name'Class)
       is null;

   overriding procedure Entry_Body_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Entry_Body_Declarations
                       .Entry_Body_Declaration'Class) is null;

   overriding procedure Entry_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Entry_Declarations
                       .Entry_Declaration'Class) is null;

   overriding procedure Generic_Subprogram_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Generic_Subprogram_Declarations
                       .Generic_Subprogram_Declaration'Class) is null;

   overriding procedure Package_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Package_Declarations
                       .Package_Declaration'Class) is null;

   overriding procedure Subprogram_Body_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Subprogram_Body_Declarations
                       .Subprogram_Body_Declaration'Class) is null;

   overriding procedure Subprogram_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Subprogram_Declarations
                       .Subprogram_Declaration'Class) is null;

   overriding procedure Subtype_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Subtype_Declarations
                       .Subtype_Declaration'Class) is null;

   overriding procedure Type_Declaration
     (Self    : access Visiter;
      Element : aliased Gela.Declarations.Type_Declarations
                       .Type_Declaration'Class) is null;

end Gela.Element_Visiters;
