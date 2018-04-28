------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision$ $Date$
--  Purpose:
--  Procedural wrapper over Object-Oriented ASIS implementation

with Gela.Element_Visiters;
with Gela.Elements.Composite_Constraints;
with Gela.Elements.Composite_Subtype_Indications;
with Gela.Elements.Scalar_Constraints;
with Gela.Elements.Scalar_Subtype_Indications;
with Gela.Elements.Simple_Expression_Range_Drs;
with Gela.Elements.Simple_Expressions;

package body Asis.Definitions is

   ---------------------------------------
   -- Access_To_Function_Result_Profile --
   ---------------------------------------

   function Access_To_Function_Result_Profile
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Type_Definition, "Access_To_Function_Result_Profile");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Access_To_Function_Result_Profile;

   ---------------------------------------
   -- Access_To_Function_Result_Subtype --
   ---------------------------------------

   function Access_To_Function_Result_Subtype
     (Definition : in Asis.Definition)
      return Asis.Definition
   is
   begin
      Check_Nil_Element (Definition, "Access_To_Function_Result_Subtype");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Access_To_Function_Result_Subtype;

   ---------------------------------
   -- Access_To_Object_Definition --
   ---------------------------------

   function Access_To_Object_Definition
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Subtype_Indication
   is
   begin
      Check_Nil_Element (Type_Definition, "Access_To_Object_Definition");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Access_To_Object_Definition;

   --------------------------------------------
   -- Access_To_Subprogram_Parameter_Profile --
   --------------------------------------------

   function Access_To_Subprogram_Parameter_Profile
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Parameter_Specification_List
   is
   begin
      Check_Nil_Element
        (Type_Definition, "Access_To_Subprogram_Parameter_Profile");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Access_To_Subprogram_Parameter_Profile;

   ---------------------------------
   -- Ancestor_Subtype_Indication --
   ---------------------------------

   function Ancestor_Subtype_Indication
     (Definition : in Asis.Definition)
      return Asis.Subtype_Indication
   is
   begin
      Check_Nil_Element (Definition, "Ancestor_Subtype_Indication");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Ancestor_Subtype_Indication;

   ---------------------------------------------
   -- Anonymous_Access_To_Object_Subtype_Mark --
   ---------------------------------------------

   function Anonymous_Access_To_Object_Subtype_Mark
     (Definition : Asis.Definition)
      return Asis.Name
   is
   begin
      Check_Nil_Element
        (Definition, "Anonymous_Access_To_Object_Subtype_Mark");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Anonymous_Access_To_Object_Subtype_Mark;

   --------------------------------
   -- Array_Component_Definition --
   --------------------------------

   function Array_Component_Definition
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Component_Definition
   is
   begin
      Check_Nil_Element (Type_Definition, "Array_Component_Definition");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Array_Component_Definition;

   ----------------------------------
   -- Component_Subtype_Indication --
   ----------------------------------

   function Component_Subtype_Indication
     (Component_Definition : in Asis.Component_Definition)
      return Asis.Subtype_Indication
   is
   begin
      Check_Nil_Element (Component_Definition, "Component_Subtype_Indication");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Component_Subtype_Indication;

   ----------------------------------
   -- Corresponding_Parent_Subtype --
   ----------------------------------

   function Corresponding_Parent_Subtype
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Type_Definition, "Corresponding_Parent_Subtype");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Parent_Subtype;

   -----------------------------
   -- Corresponding_Root_Type --
   -----------------------------

   function Corresponding_Root_Type
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Type_Definition, "Corresponding_Root_Type");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Root_Type;

   ----------------------------------
   -- Corresponding_Type_Operators --
   ----------------------------------

   function Corresponding_Type_Operators
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Declaration_List
   is
   begin
      Check_Nil_Element (Type_Definition, "Corresponding_Type_Operators");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Corresponding_Type_Operators;

   ----------------------------------
   -- Corresponding_Type_Structure --
   ----------------------------------

   function Corresponding_Type_Structure
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Type_Definition, "Corresponding_Type_Structure");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Type_Structure;

   ----------------------
   -- Delta_Expression --
   ----------------------

   function Delta_Expression
     (Definition : in Asis.Definition)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Definition, "Delta_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Delta_Expression;

   -----------------------
   -- Digits_Expression --
   -----------------------

   function Digits_Expression
     (Definition : in Asis.Definition)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Definition, "Digits_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Digits_Expression;

   ---------------------
   -- Discrete_Ranges --
   ---------------------

   function Discrete_Ranges
     (Constraint : in Asis.Constraint)
      return Asis.Discrete_Range_List
   is
   begin
      Check_Nil_Element (Constraint, "Discrete_Ranges");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Discrete_Ranges;

   ----------------------------------
   -- Discrete_Subtype_Definitions --
   ----------------------------------

   function Discrete_Subtype_Definitions
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Definition_List
   is
   begin
      Check_Nil_Element (Type_Definition, "Discrete_Subtype_Definitions");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Discrete_Subtype_Definitions;

   -------------------------------
   -- Discriminant_Associations --
   -------------------------------

   function Discriminant_Associations
     (Constraint : in Asis.Constraint;
      Normalized : in Boolean := False)
      return Asis.Discriminant_Association_List
   is
   begin
      Check_Nil_Element (Constraint, "Discriminant_Associations");
      if Normalized then
         Raise_Not_Implemented ("");
         return Asis.Nil_Element_List;
      else
         Raise_Not_Implemented ("");
         return Asis.Nil_Element_List;
      end if;
   end Discriminant_Associations;

   ------------------------------
   -- Discriminant_Direct_Name --
   ------------------------------

   function Discriminant_Direct_Name
     (Variant_Part : in Asis.Record_Component)
      return Asis.Name
   is
   begin
      Check_Nil_Element (Variant_Part, "Discriminant_Direct_Name");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Discriminant_Direct_Name;

   -------------------
   -- Discriminants --
   -------------------

   function Discriminants
     (Definition : in Asis.Definition)
      return Asis.Discriminant_Specification_List
   is
   begin
      Check_Nil_Element (Definition, "Discriminants");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Discriminants;

   --------------------------------------
   -- Enumeration_Literal_Declarations --
   --------------------------------------

   function Enumeration_Literal_Declarations
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Declaration_List
   is
   begin
      Check_Nil_Element (Type_Definition, "Enumeration_Literal_Declarations");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Enumeration_Literal_Declarations;

   -------------------------
   -- Implicit_Components --
   -------------------------

   function Implicit_Components
     (Definition : in Asis.Definition)
      return Asis.Record_Component_List
   is
   begin
      Check_Nil_Element (Definition, "Implicit_Components");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Implicit_Components;

   -------------------------------------
   -- Implicit_Inherited_Declarations --
   -------------------------------------

   function Implicit_Inherited_Declarations
     (Definition : in Asis.Definition)
      return Asis.Declaration_List
   is
   begin
      Check_Nil_Element (Definition, "Implicit_Inherited_Declarations");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Implicit_Inherited_Declarations;

   ------------------------------------
   -- Implicit_Inherited_Subprograms --
   ------------------------------------

   function Implicit_Inherited_Subprograms
     (Definition : in Asis.Definition)
      return Asis.Declaration_List
   is
   begin
      Check_Nil_Element (Definition, "Implicit_Inherited_Subprograms");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Implicit_Inherited_Subprograms;

   -------------------------------
   -- Index_Subtype_Definitions --
   -------------------------------

   function Index_Subtype_Definitions
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Expression_List
   is
   begin
      Check_Nil_Element (Type_Definition, "Index_Subtype_Definitions");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Index_Subtype_Definitions;

   ------------------------
   -- Integer_Constraint --
   ------------------------

   function Integer_Constraint
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Range_Constraint
   is
   begin
      Check_Nil_Element (Type_Definition, "Integer_Constraint");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Integer_Constraint;

   ------------------------
   -- Is_Private_Present --
   ------------------------

   function Is_Private_Present
     (Definition : in Asis.Definition)
      return Boolean
   is
   begin
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Is_Private_Present;

   --------------------------------
   -- Is_Task_Definition_Present --
   --------------------------------

   function Is_Task_Definition_Present
     (Definition : in Asis.Definition)
      return Boolean
   is
   begin
      return Assigned (Definition) and then
         Is_Task_Definition_Present (Definition);
   end Is_Task_Definition_Present;

   -----------------
   -- Lower_Bound --
   -----------------

   function Lower_Bound
     (Constraint : in Asis.Range_Constraint)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Constraint, "Lower_Bound");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Lower_Bound;

   ---------------------------
   -- Mod_Static_Expression --
   ---------------------------

   function Mod_Static_Expression
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Type_Definition, "Mod_Static_Expression");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Mod_Static_Expression;

   -------------------------------
   -- Parent_Subtype_Indication --
   -------------------------------

   function Parent_Subtype_Indication
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Subtype_Indication
   is
   begin
      Check_Nil_Element (Type_Definition, "Parent_Subtype_Indication");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Parent_Subtype_Indication;

   ------------------------
   -- Private_Part_Items --
   ------------------------

   function Private_Part_Items
     (Definition : in Asis.Definition;
      Include_Pragmas : in Boolean := False)
      return Asis.Declarative_Item_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Definition, "Private_Part_Items");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Private_Part_Items;

   ---------------------
   -- Progenitor_List --
   ---------------------

   function Progenitor_List
     (Type_Definition : Asis.Definition)
     return Asis.Name_List is
   begin
      Check_Nil_Element (Type_Definition, "Progenitor_List");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Progenitor_List;

   ---------------------
   -- Range_Attribute --
   ---------------------

   function Range_Attribute
     (Constraint : in Asis.Range_Constraint)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Constraint, "Range_Attribute");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Range_Attribute;

   ---------------------------
   -- Real_Range_Constraint --
   ---------------------------

   function Real_Range_Constraint
     (Definition : in Asis.Definition)
      return Asis.Range_Constraint
   is
   begin
      Check_Nil_Element (Definition, "Real_Range_Constraint");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Real_Range_Constraint;

   -----------------------
   -- Record_Components --
   -----------------------

   function Record_Components
     (Definition : in Asis.Definition;
      Include_Pragmas : in Boolean := False)
      return Asis.Record_Component_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Definition, "Record_Components");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Record_Components;

   -----------------------
   -- Record_Definition --
   -----------------------

   function Record_Definition
     (Type_Definition : in Asis.Type_Definition)
      return Asis.Definition
   is
   begin
      Check_Nil_Element (Type_Definition, "Record_Definition");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Record_Definition;

   ------------------------
   -- Subtype_Constraint --
   ------------------------

   function Subtype_Constraint
     (Definition : in Asis.Definition) return Asis.Constraint
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Composite_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Subtype_Indications.
              Composite_Subtype_Indication_Access);

         overriding procedure Scalar_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Scalar_Subtype_Indications.
              Scalar_Subtype_Indication_Access);
      end Get;

      package body Get is

         overriding procedure Composite_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Subtype_Indications.
              Composite_Subtype_Indication_Access)
         is
            X : constant Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access := Node.Composite_Constraint;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Composite_Subtype_Indication;

         overriding procedure Scalar_Subtype_Indication
           (Self : in out Visiter;
            Node : not null Gela.Elements.Scalar_Subtype_Indications.
              Scalar_Subtype_Indication_Access)
         is
            X : constant Gela.Elements.Scalar_Constraints.
              Scalar_Constraint_Access := Node.Scalar_Constraint;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Scalar_Subtype_Indication;
      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Definition, "Subtype_Constraint");
      Definition.Data.Visit (V);

      return (Data => V.Result);
   end Subtype_Constraint;

   ------------------
   -- Subtype_Mark --
   ------------------

   function Subtype_Mark
     (Definition : in Asis.Definition)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Definition, "Subtype_Mark");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Subtype_Mark;

   -----------------
   -- Upper_Bound --
   -----------------

   function Upper_Bound
     (Constraint : in Asis.Range_Constraint)
      return Asis.Expression
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Simple_Expression_Range_Dr
           (Self : in out Visiter;
            Node : not null Gela.Elements.Simple_Expression_Range_Drs.
              Simple_Expression_Range_Dr_Access);
      end Get;

      package body Get is

         overriding procedure Simple_Expression_Range_Dr
           (Self : in out Visiter;
            Node : not null Gela.Elements.Simple_Expression_Range_Drs.
              Simple_Expression_Range_Dr_Access)
         is
            X : constant Gela.Elements.Simple_Expressions.
              Simple_Expression_Access := Node.Upper_Bound;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Simple_Expression_Range_Dr;
      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Constraint, "Upper_Bound");
      Constraint.Data.Visit (V);

      return (Data => V.Result);
   end Upper_Bound;

   ---------------------
   -- Variant_Choices --
   ---------------------

   function Variant_Choices
     (Variant : in Asis.Variant)
      return Asis.Element_List
   is
   begin
      Check_Nil_Element (Variant, "Variant_Choices");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Variant_Choices;

   --------------
   -- Variants --
   --------------

   function Variants
     (Variant_Part    : in Asis.Record_Component;
      Include_Pragmas : in Boolean := False)
      return Asis.Variant_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Variant_Part, "Variants");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Variants;

   ------------------------
   -- Visible_Part_Items --
   ------------------------

   function Visible_Part_Items
     (Definition : in Asis.Definition;
      Include_Pragmas : in Boolean := False)
      return Asis.Declarative_Item_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Definition, "Visible_Part_Items");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Visible_Part_Items;

end Asis.Definitions;


------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
