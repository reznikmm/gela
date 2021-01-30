--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Names;
with Program.Elements.Defining_Operator_Symbols;
with Program.Elements.Function_Declarations;
with Program.Elements.Identifiers;
with Program.Elements.Parameter_Specifications;
with Program.Symbols;

package body Program.Predefined_Operators is

   --------------------------------
   -- Create_Operators_For_Array --
   --------------------------------

   procedure Create_Operators_For_Array
     (Self      : in out Program.Visibility.Context'Class;
      Type_View : Program.Visibility.View;
      Setter    : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Factory   : Program.Implicit_Element_Factories.Element_Factory;
      Vectors   : Program.Element_Vector_Factories.Element_Vector_Factory;
      Result    : out Program.Element_Vectors.Element_Vector_Access)
   is
      Type_Name : constant Program.Elements.Defining_Names.Defining_Name_Access
        := Program.Visibility.Name (Type_View);
      Indexes : constant Program.Visibility.View_Array :=
        Program.Visibility.Indexes (Type_View);
      Name : Program.Elements.Defining_Operator_Symbols
        .Defining_Operator_Symbol_Access;
      Names : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
      Left  : Program.Elements.Defining_Identifiers.Defining_Identifier_Access;
      Right : Program.Elements.Defining_Identifiers.Defining_Identifier_Access;
      Tipe  : Program.Elements.Identifiers.Identifier_Access;

      PL, PR : Program.Elements.Parameter_Specifications
        .Parameter_Specification_Access;

      List  : Program.Elements.Parameter_Specifications
        .Parameter_Specification_Vector_Access;

      Decl : Program.Elements.Function_Declarations
        .Function_Declaration_Access;
   begin
      if Indexes'Length /= 1 then  --  FIXME: Check nonlimited
         return;
      end if;

      Name := Factory.Create_Defining_Operator_Symbol
        (Is_Part_Of_Implicit => True);

      Left := Factory.Create_Defining_Identifier (Is_Part_Of_Implicit => True);

      Names := Vectors.Create_Defining_Identifier_Vector
        (Program.Element_Vectors.Single_Element
          (Program.Elements.Element_Access (Left)));

      Tipe := Factory.Create_Identifier (Is_Part_Of_Implicit => True);

      Setter.Set_Corresponding_Defining_Name
        (Program.Elements.Element_Access (Tipe), Type_Name);

      PL := Factory.Create_Parameter_Specification
        (Names                => Names,
         Parameter_Subtype    => Program.Elements.Element_Access (Tipe),
         Default_Expression   => null,
         Is_Part_Of_Implicit  => True);

      Right := Factory.Create_Defining_Identifier
        (Is_Part_Of_Implicit => True);

      Names := Vectors.Create_Defining_Identifier_Vector
        (Program.Element_Vectors.Single_Element
          (Program.Elements.Element_Access (Right)));

      Tipe := Factory.Create_Identifier (Is_Part_Of_Implicit => True);

      Setter.Set_Corresponding_Defining_Name
        (Program.Elements.Element_Access (Tipe), Type_Name);

      PR := Factory.Create_Parameter_Specification
        (Names                => Names,
         Parameter_Subtype    => Program.Elements.Element_Access (Tipe),
         Default_Expression   => null,
         Is_Part_Of_Implicit  => True);

      List := Vectors.Create_Parameter_Specification_Vector
        (Program.Element_Vectors.Two_Elements
          (Program.Elements.Element_Access (PL),
           Program.Elements.Element_Access (PR)));

      Tipe := Factory.Create_Identifier (Is_Part_Of_Implicit => True);

      Setter.Set_Corresponding_Defining_Name
        (Program.Elements.Element_Access (Tipe), Type_Name);

      Decl := Factory.Create_Function_Declaration
        (Name                => Name.all'Access,
         Parameters          => List,
         Result_Subtype      => Program.Elements.Element_Access (Tipe),
         Result_Expression   => null,
         Aspects             => null,
         Is_Part_Of_Implicit => True);

      Self.Create_Function
        (Symbol => Program.Symbols.Ampersand_Symbol,
         Name   => Name.all'Access);

      Self.Create_Parameter
        (Symbol      => Program.Symbols.Left,
         Name        => Left.all'Access,
         Mode        => Program.Visibility.In_Mode,
         Has_Default => False);

      Self.Leave_Declarative_Region;
      Self.Set_Parameter_Type (Type_View);

      Self.Create_Parameter
        (Symbol      => Program.Symbols.Right,
         Name        => Right.all'Access,
         Mode        => Program.Visibility.In_Mode,
         Has_Default => False);

      Self.Leave_Declarative_Region;
      Self.Set_Parameter_Type (Type_View);

      Result := Vectors.Create_Element_Vector
        (Program.Element_Vectors.Single_Element
         (Program.Elements.Element_Access (Decl)));
   end Create_Operators_For_Array;

end Program.Predefined_Operators;
