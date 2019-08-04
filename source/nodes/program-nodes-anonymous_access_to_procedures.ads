--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Anonymous_Access_To_Procedures;
with Program.Element_Visitors;

package Program.Nodes.Anonymous_Access_To_Procedures is

   pragma Pure (Program.Nodes.Anonymous_Access_To_Procedures);

   type Anonymous_Access_To_Procedure is
     new Program.Nodes.Node
         and Program.Elements.Anonymous_Access_To_Procedures
           .Anonymous_Access_To_Procedure
         and Program.Elements.Anonymous_Access_To_Procedures
           .Anonymous_Access_To_Procedure_Text
     with private;

   function Create
    (Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Anonymous_Access_To_Procedure;

   type Implicit_Anonymous_Access_To_Procedure is
     new Program.Nodes.Node
         and Program.Elements.Anonymous_Access_To_Procedures
           .Anonymous_Access_To_Procedure
     with private;

   function Create
    (Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_Protected        : Boolean := False)
      return Implicit_Anonymous_Access_To_Procedure
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Anonymous_Access_To_Procedure is
     abstract new Program.Nodes.Node
       and Program.Elements.Anonymous_Access_To_Procedures
         .Anonymous_Access_To_Procedure
     with record
        Parameters : not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Anonymous_Access_To_Procedure'Class);

   overriding procedure Visit
    (Self    : not null access Base_Anonymous_Access_To_Procedure;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Parameters
    (Self : Base_Anonymous_Access_To_Procedure)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Is_Anonymous_Access_To_Procedure
    (Self : Base_Anonymous_Access_To_Procedure)
      return Boolean;

   overriding function Is_Anonymous_Access_Definition
    (Self : Base_Anonymous_Access_To_Procedure)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Anonymous_Access_To_Procedure)
      return Boolean;

   type Anonymous_Access_To_Procedure is
     new Base_Anonymous_Access_To_Procedure
       and Program.Elements.Anonymous_Access_To_Procedures
         .Anonymous_Access_To_Procedure_Text
     with record
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Access_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Procedure_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Anonymous_Access_To_Procedure_Text
    (Self : aliased in out Anonymous_Access_To_Procedure)
      return Program.Elements.Anonymous_Access_To_Procedures
          .Anonymous_Access_To_Procedure_Text_Access;

   overriding function Not_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Access_Token
    (Self : Anonymous_Access_To_Procedure)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Protected_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Procedure_Token
    (Self : Anonymous_Access_To_Procedure)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null
    (Self : Anonymous_Access_To_Procedure)
      return Boolean;

   overriding function Has_Protected
    (Self : Anonymous_Access_To_Procedure)
      return Boolean;

   type Implicit_Anonymous_Access_To_Procedure is
     new Base_Anonymous_Access_To_Procedure
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
        Has_Protected        : Boolean;
     end record;

   overriding function To_Anonymous_Access_To_Procedure_Text
    (Self : aliased in out Implicit_Anonymous_Access_To_Procedure)
      return Program.Elements.Anonymous_Access_To_Procedures
          .Anonymous_Access_To_Procedure_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean;

   overriding function Has_Protected
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean;

end Program.Nodes.Anonymous_Access_To_Procedures;
