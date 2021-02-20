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

   package E renames Program.Elements;

   procedure Create_Operator
     (Self      : in out Program.Visibility.Context'Class;
      Symbol    : Program.Symbols.Symbol;
      Type_View : Program.Visibility.View;
      Setter    : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Factory   : Program.Implicit_Element_Factories.Element_Factory;
      Vectors   : Program.Element_Vector_Factories.Element_Vector_Factory;
      Decl      : out E.Function_Declarations.Function_Declaration_Access);

   ---------------------
   -- Create_Operator --
   ---------------------

   procedure Create_Operator
     (Self      : in out Program.Visibility.Context'Class;
      Symbol    : Program.Symbols.Symbol;
      Type_View : Program.Visibility.View;
      Setter    : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Factory   : Program.Implicit_Element_Factories.Element_Factory;
      Vectors   : Program.Element_Vector_Factories.Element_Vector_Factory;
      Decl      : out E.Function_Declarations.Function_Declaration_Access)
   is
      procedure New_Parameter
        (Spec : out E.Parameter_Specifications.Parameter_Specification_Access);

      -------------------
      -- New_Parameter --
      -------------------

      procedure New_Parameter
        (Spec : out E.Parameter_Specifications.Parameter_Specification_Access)
      is
         Type_Name : constant E.Defining_Names.Defining_Name_Access :=
           Program.Visibility.Name (Type_View);

         Vector : E.Defining_Identifiers.Defining_Identifier_Vector_Access;

         Tipe_Id  : constant E.Identifiers.Identifier_Access :=
           Factory.Create_Identifier (Is_Part_Of_Implicit => True);

         Id  : constant E.Defining_Identifiers.Defining_Identifier_Access :=
           Factory.Create_Defining_Identifier (Is_Part_Of_Implicit => True);
      begin
         Setter.Set_Corresponding_Defining_Name
           (E.Element_Access (Tipe_Id), Type_Name);

         Vector := Vectors.Create_Defining_Identifier_Vector
           (Program.Element_Vectors.Single_Element
              (E.Element_Access (Id)));

         Spec := Factory.Create_Parameter_Specification
           (Names                => Vector,
            Parameter_Subtype    => E.Element_Access (Tipe_Id),
            Default_Expression   => null,
            Is_Part_Of_Implicit  => True);

         Self.Create_Parameter
           (Symbol      => Program.Symbols.Left,
            Name        => Id.all'Access,
            Mode        => Program.Visibility.In_Mode,
            Has_Default => False);

         Self.Leave_Declarative_Region;
         Self.Set_Object_Type (Type_View);
      end New_Parameter;

      Type_Name : constant E.Defining_Names.Defining_Name_Access :=
        Program.Visibility.Name (Type_View);

      Tipe : constant E.Identifiers.Identifier_Access :=
        Factory.Create_Identifier (Is_Part_Of_Implicit => True);

      Name : constant E.Defining_Operator_Symbols
          .Defining_Operator_Symbol_Access :=
             Factory.Create_Defining_Operator_Symbol
               (Is_Part_Of_Implicit => True);

      Left : E.Parameter_Specifications.Parameter_Specification_Access;
      Right : E.Parameter_Specifications.Parameter_Specification_Access;

      List  : E.Parameter_Specifications.Parameter_Specification_Vector_Access;

   begin
      Self.Create_Function (Symbol, Name.all'Access);

      New_Parameter (Left);
      New_Parameter (Right);

      Self.Set_Result_Type (Type_View);
      Self.Leave_Declarative_Region;

      List := Vectors.Create_Parameter_Specification_Vector
        (Program.Element_Vectors.Two_Elements
          (E.Element_Access (Left),
           E.Element_Access (Right)));

      Setter.Set_Corresponding_Defining_Name
        (E.Element_Access (Tipe), Type_Name);

      Decl := Factory.Create_Function_Declaration
        (Name                => Name.all'Access,
         Parameters          => List,
         Result_Subtype      => E.Element_Access (Tipe),
         Result_Expression   => null,
         Aspects             => null,
         Is_Part_Of_Implicit => True);

   end Create_Operator;

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
      Indexes : constant Program.Visibility.View_Array :=
        Program.Visibility.Indexes (Type_View);

      Ampersand : E.Function_Declarations.Function_Declaration_Access;
   begin
      if Indexes'Length /= 1 then  --  FIXME: Check nonlimited
         return;
      end if;

      Create_Operator
        (Self,
         Symbol    => Program.Symbols.Ampersand_Symbol,
         Type_View => Type_View,
         Setter    => Setter,
         Factory   => Factory,
         Vectors   => Vectors,
         Decl      => Ampersand);

      Result := Vectors.Create_Element_Vector
        (Program.Element_Vectors.Single_Element
           (E.Element_Access (Ampersand)));
   end Create_Operators_For_Array;

   ----------------------------------
   -- Create_Operators_For_Integer --
   ----------------------------------

   procedure Create_Operators_For_Integer
     (Self      : in out Program.Visibility.Context'Class;
      Type_View : Program.Visibility.View;
      Setter    : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Factory   : Program.Implicit_Element_Factories.Element_Factory;
      Vectors   : Program.Element_Vector_Factories.Element_Vector_Factory;
      Result    : out Program.Element_Vectors.Element_Vector_Access)
   is
      Hyphen : E.Function_Declarations.Function_Declaration_Access;
   begin
      Create_Operator
        (Self,
         Symbol    => Program.Symbols.Hyphen_Symbol,
         Type_View => Type_View,
         Setter    => Setter,
         Factory   => Factory,
         Vectors   => Vectors,
         Decl      => Hyphen);

      Result := Vectors.Create_Element_Vector
        (Program.Element_Vectors.Single_Element
           (E.Element_Access (Hyphen)));
   end Create_Operators_For_Integer;

end Program.Predefined_Operators;
