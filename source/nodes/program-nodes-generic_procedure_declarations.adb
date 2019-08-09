--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Generic_Procedure_Declarations is

   function Create
    (Generic_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters   : Program.Element_Vectors.Element_Vector_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Generic_Procedure_Declaration is
   begin
      return Result : Generic_Procedure_Declaration :=
        (Generic_Token => Generic_Token,
         Formal_Parameters => Formal_Parameters,
         Procedure_Token => Procedure_Token, Name => Name,
         Left_Bracket_Token => Left_Bracket_Token, Parameters => Parameters,
         Right_Bracket_Token => Right_Bracket_Token, With_Token => With_Token,
         Aspects => Aspects, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Formal_Parameters    : Program.Element_Vectors.Element_Vector_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Generic_Procedure_Declaration is
   begin
      return Result : Implicit_Generic_Procedure_Declaration :=
        (Formal_Parameters => Formal_Parameters, Name => Name,
         Parameters => Parameters, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Formal_Parameters
    (Self : Base_Generic_Procedure_Declaration)
      return Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Formal_Parameters;
   end Formal_Parameters;

   overriding function Name
    (Self : Base_Generic_Procedure_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Parameters
    (Self : Base_Generic_Procedure_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Aspects
    (Self : Base_Generic_Procedure_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Generic_Token
    (Self : Generic_Procedure_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Generic_Token;
   end Generic_Token;

   overriding function Procedure_Token
    (Self : Generic_Procedure_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Procedure_Token;
   end Procedure_Token;

   overriding function Left_Bracket_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function With_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Generic_Procedure_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Generic_Procedure_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Generic_Procedure_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Generic_Procedure_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Generic_Procedure_Declaration'Class) is
   begin
      for Item in Self.Formal_Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Generic_Procedure_Declaration_Element
    (Self : Base_Generic_Procedure_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Generic_Procedure_Declaration_Element;

   overriding function Is_Declaration_Element
    (Self : Base_Generic_Procedure_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration_Element;

   overriding procedure Visit
    (Self    : not null access Base_Generic_Procedure_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Generic_Procedure_Declaration (Self);
   end Visit;

   overriding function To_Generic_Procedure_Declaration_Text
    (Self : aliased in out Generic_Procedure_Declaration)
      return Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Generic_Procedure_Declaration_Text;

   overriding function To_Generic_Procedure_Declaration_Text
    (Self : aliased in out Implicit_Generic_Procedure_Declaration)
      return Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Generic_Procedure_Declaration_Text;

end Program.Nodes.Generic_Procedure_Declarations;
