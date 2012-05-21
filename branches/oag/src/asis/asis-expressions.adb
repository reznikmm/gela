------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-expressions.adb 2455 2006-06-24 19:22:06Z maxr $

--with XASIS.Utils;

with Asis.Elements;
with Asis.Gela.Elements; use Asis.Gela.Elements;

package body Asis.Expressions is
   use Asis.Gela;

   ----------------------
   -- Actual_Parameter --
   ----------------------

   function Actual_Parameter
     (Association : in Asis.Association)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Association, "Actual_Parameter");
      return Child_Element (Association, P.Actual_Parameter);
   end Actual_Parameter;

   ------------------------------------
   -- Allocator_Qualified_Expression --
   ------------------------------------

   function Allocator_Qualified_Expression
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Allocator_Qualified_Expression");
      return Child_Element (Expression, P.Allocator_Qualified_Expression);
   end Allocator_Qualified_Expression;

   ----------------------------------
   -- Allocator_Subtype_Indication --
   ----------------------------------

   function Allocator_Subtype_Indication
     (Expression : in Asis.Expression)
      return Asis.Subtype_Indication
   is
   begin
      Check_Nil_Element (Expression, "Allocator_Subtype_Indication");
      return Child_Element (Expression, P.Allocator_Subtype_Indication);
   end Allocator_Subtype_Indication;

   ----------------------------------
   -- Array_Component_Associations --
   ----------------------------------

   function Array_Component_Associations
     (Expression : in Asis.Expression)
      return Asis.Association_List
   is
   begin
      Check_Nil_Element (Expression, "Array_Component_Associations");
      return Child_Elements (Expression, P.Array_Component_Associations);
   end Array_Component_Associations;

   -----------------------------
   -- Array_Component_Choices --
   -----------------------------

   function Array_Component_Choices
     (Association : in Asis.Association)
      return Asis.Expression_List
   is
   begin
      Check_Nil_Element (Association, "Array_Component_Choices");
      return Child_Elements (Association, P.Array_Component_Choices);
   end Array_Component_Choices;

   --------------------------------------
   -- Attribute_Designator_Expressions --
   --------------------------------------

   function Attribute_Designator_Expressions
     (Expression : in Asis.Expression)
      return Asis.Expression_List
   is
   begin
      Check_Nil_Element (Expression, "Attribute_Designator_Expressions");
      return Child_Elements (Expression, P.Attribute_Designator_Expressions);
   end Attribute_Designator_Expressions;

   -------------------------------------
   -- Attribute_Designator_Identifier --
   -------------------------------------

   function Attribute_Designator_Identifier
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Attribute_Designator_Identifier");
      return Child_Element (Expression, P.Attribute_Designator_Identifier);
   end Attribute_Designator_Identifier;

   --------------------------
   -- Component_Expression --
   --------------------------

   function Component_Expression
     (Association : in Asis.Association)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Association, "Component_Expression");
      return Child_Element (Association, P.Component_Expression);
   end Component_Expression;

   ---------------------------------------
   -- Converted_Or_Qualified_Expression --
   ---------------------------------------

   function Converted_Or_Qualified_Expression
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Converted_Or_Qualified_Expression");
      return Child_Element (Expression, P.Converted_Or_Qualified_Expression);
   end Converted_Or_Qualified_Expression;

   -----------------------------------------
   -- Converted_Or_Qualified_Subtype_Mark --
   -----------------------------------------

   function Converted_Or_Qualified_Subtype_Mark
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Converted_Or_Qualified_Subtype_Mark");
      return Child_Element (Expression, P.Converted_Or_Qualified_Subtype_Mark);
   end Converted_Or_Qualified_Subtype_Mark;

   -----------------------------------
   -- Corresponding_Called_Function --
   -----------------------------------

   function Corresponding_Called_Function
     (Expression : in Asis.Expression)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Expression, "Corresponding_Called_Function");
      raise Not_Implemented_Error;
      return Nil_Element;
   end Corresponding_Called_Function;

   -----------------------------------
   -- Corresponding_Expression_Type --
   -----------------------------------

   function Corresponding_Expression_Type
     (Expression : in Asis.Expression)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Expression, "Corresponding_Expression_Type");
      raise Not_Implemented_Error;
      return Nil_Element;
   end Corresponding_Expression_Type;

   ------------------------------------
   -- Corresponding_Name_Declaration --
   ------------------------------------

   function Corresponding_Name_Declaration
     (Reference : in Asis.Expression)
      return Asis.Element
   is
   begin
      Check_Nil_Element (Reference, "Corresponding_Name_Declaration");
      raise Not_Implemented_Error;
      return Nil_Element;
   end Corresponding_Name_Declaration;

   -----------------------------------
   -- Corresponding_Name_Definition --
   -----------------------------------

   function Corresponding_Name_Definition
     (Reference : in Asis.Expression)
      return Asis.Defining_Name
   is
      List : constant Asis.Defining_Name_List :=
        Corresponding_Name_Definition_List (Reference);
   begin
      if List'Length = 0 then
         return Nil_Element;
      else
         return List (List'First);
      end if;
   end Corresponding_Name_Definition;

   ----------------------------------------
   -- Corresponding_Name_Definition_List --
   ----------------------------------------

   function Corresponding_Name_Definition_List
     (Reference : in Asis.Element)
      return Asis.Defining_Name_List
   is
   begin
      Check_Nil_Element (Reference, "Corresponding_Name_Definition_List");
      raise Not_Implemented_Error;
      return Nil_Element_List;
   end Corresponding_Name_Definition_List;

   -----------------------------
   -- Discriminant_Expression --
   -----------------------------

   function Discriminant_Expression
     (Association : in Asis.Discriminant_Association)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Association, "Discriminant_Expression");
      return Child_Element (Association, P.Discriminant_Expression);
   end Discriminant_Expression;

   ---------------------------------
   -- Discriminant_Selector_Names --
   ---------------------------------

   function Discriminant_Selector_Names
     (Association : in Asis.Discriminant_Association)
      return Asis.Expression_List
   is
   begin
      Check_Nil_Element (Association, "Discriminant_Selector_Names");
      return Child_Elements (Association, P.Discriminant_Selector_Names);
   end Discriminant_Selector_Names;

   ------------------------------
   -- Expression_Parenthesized --
   ------------------------------

   function Expression_Parenthesized
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Expression_Parenthesized");
      return Child_Element (Expression, P.Expression_Parenthesized);
   end Expression_Parenthesized;

   ------------------------------------
   -- Extension_Aggregate_Expression --
   ------------------------------------

   function Extension_Aggregate_Expression
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Extension_Aggregate_Expression");
      return Child_Element (Expression, P.Extension_Aggregate_Expression);
   end Extension_Aggregate_Expression;

   ----------------------
   -- Formal_Parameter --
   ----------------------

   function Formal_Parameter
     (Association : in Asis.Association)
      return Asis.Element
   is
   begin
      Check_Nil_Element (Association, "Formal_Parameter");
      return Child_Element (Association, P.Formal_Parameter);
   end Formal_Parameter;

   ------------------------------
   -- Function_Call_Parameters --
   ------------------------------

   function Function_Call_Parameters
     (Expression : in Asis.Expression;
      Normalized : in Boolean := False)
      return Asis.Association_List
   is
   begin
      Check_Nil_Element (Expression, "Function_Call_Parameters");
      if Normalized then
         raise Not_Implemented_Error;
         return Child_Elements
           (Expression, P.Normalized_Function_Call_Parameters);
      else
         return Child_Elements (Expression, P.Function_Call_Parameters);
      end if;
   end Function_Call_Parameters;

   -----------------------
   -- Index_Expressions --
   -----------------------

   function Index_Expressions
     (Expression : in Asis.Expression)
      return Asis.Expression_List
   is
   begin
      Check_Nil_Element (Expression, "Index_Expressions");
      return Child_Elements (Expression, P.Index_Expressions);
   end Index_Expressions;

   ------------------------------
   -- Is_Defaulted_Association --
   ------------------------------

   function Is_Defaulted_Association
     (Association : in Asis.Association)
      return Boolean
   is
   begin
      Check_Nil_Element (Association, "Is_Defaulted_Association");
      return Get_Boolean (Association, P.Is_Defaulted_Association);
   end Is_Defaulted_Association;

   -------------------
   -- Is_Normalized --
   -------------------

   function Is_Normalized
     (Association : in Asis.Association)
      return Boolean
   is
   begin
      Check_Nil_Element (Association, "Is_Normalized");
      return Get_Boolean (Association, P.Is_Normalized);
   end Is_Normalized;

   --------------------
   -- Is_Prefix_Call --
   --------------------

   function Is_Prefix_Call
     (Expression : in Asis.Expression)
      return Boolean
   is
   begin
      Check_Nil_Element (Expression, "Is_Prefix_Call");
      return Get_Boolean (Expression, P.Is_Prefix_Call);
   end Is_Prefix_Call;

   -------------------
   -- Is_Referenced --
   -------------------

   function Is_Referenced
     (Name           : in Asis.Element;
      Within_Element : in Asis.Element;
      Implicitly     : in Boolean := False)
      return Boolean
   is
   begin
      return References (Name, Within_Element, Implicitly)'Length > 0;
   end Is_Referenced;

   --------------------------------
   -- Membership_Test_Expression --
   --------------------------------

   function Membership_Test_Expression
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Membership_Test_Expression");
      return Child_Element (Expression, P.Membership_Test_Expression);
   end Membership_Test_Expression;

   ---------------------------
   -- Membership_Test_Range --
   ---------------------------

   function Membership_Test_Range
     (Expression : in Asis.Expression)
      return Asis.Range_Constraint
   is
   begin
      Check_Nil_Element (Expression, "Membership_Test_Range");
      return Child_Element (Expression, P.Membership_Test_Range);
   end Membership_Test_Range;

   ----------------------------------
   -- Membership_Test_Subtype_Mark --
   ----------------------------------

   function Membership_Test_Subtype_Mark
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Membership_Test_Subtype_Mark");
      return Child_Element (Expression, P.Membership_Test_Subtype_Mark);
   end Membership_Test_Subtype_Mark;

   ----------------
   -- Name_Image --
   ----------------

   function Name_Image
     (Expression : in Asis.Expression)
      return Program_Text
   is
   begin
      Check_Nil_Element (Expression, "Name_Image");
      return Get_Image (Expression, P.Name_Image);
   end Name_Image;

   ------------
   -- Prefix --
   ------------

   function Prefix
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Prefix");
      return Child_Element (Expression, P.Prefix);
   end Prefix;

   -----------------------------------
   -- Record_Component_Associations --
   -----------------------------------

   function Record_Component_Associations
     (Expression : in Asis.Expression;
      Normalized : in Boolean := False)
      return Asis.Association_List
   is
   begin
      Check_Nil_Element (Expression, "Record_Component_Associations");
      if Normalized then
         raise Not_Implemented_Error;
         return Child_Elements
           (Expression, P.Normalized_Record_Component_Associations);
      else
         return Child_Elements (Expression, P.Record_Component_Associations);
      end if;
   end Record_Component_Associations;

   ------------------------------
   -- Record_Component_Choices --
   ------------------------------

   function Record_Component_Choices
     (Association : in Asis.Association)
      return Asis.Expression_List
   is
   begin
      Check_Nil_Element (Association, "Record_Component_Choices");
      return Child_Elements (Association, P.Record_Component_Choices);
   end Record_Component_Choices;

   ----------------
   -- References --
   ----------------

   function References
     (Name           : in Asis.Element;
      Within_Element : in Asis.Element;
      Implicitly     : in Boolean := False)
      return Asis.Name_List
   is
      use Asis.Elements;
      Result : Asis.Name_List := Child_Elements (Name, P.References, True);
      Last   : Asis.List_Index := 1;
   begin
      for I in Result'Range loop
         if (Implicitly or else not Is_Part_Of_Implicit (Result (I)))
-- TODO       and then XASIS.Utils.Is_Child_Of (Result (I), Within_Element)
         then
           Result (Last) := Result (I);
           Last := Last + 1;
         end if;
      end loop;

      raise Not_Implemented_Error;
      return Result (1 .. Last - 1);
   end References;

   --------------
   -- Selector --
   --------------

   function Selector
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Expression, "Selector");
      return Child_Element (Expression, P.Selector);
   end Selector;

   ---------------------------------------------
   -- Short_Circuit_Operation_Left_Expression --
   ---------------------------------------------

   function Short_Circuit_Operation_Left_Expression
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element
        (Expression, "Short_Circuit_Operation_Left_Expression");

      return Child_Element
        (Expression, P.Short_Circuit_Operation_Left_Expression);
   end Short_Circuit_Operation_Left_Expression;

   ----------------------------------------------
   -- Short_Circuit_Operation_Right_Expression --
   ----------------------------------------------

   function Short_Circuit_Operation_Right_Expression
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
   begin
      Check_Nil_Element
        (Expression, "Short_Circuit_Operation_Right_Expression");

      return Child_Element
        (Expression, P.Short_Circuit_Operation_Right_Expression);
   end Short_Circuit_Operation_Right_Expression;

   -----------------
   -- Slice_Range --
   -----------------

   function Slice_Range
     (Expression : in Asis.Expression)
      return Asis.Discrete_Range
   is
   begin
      Check_Nil_Element (Expression, "Slice_Range");
      return Child_Element (Expression, P.Slice_Range);
   end Slice_Range;

   -----------------
   -- Value_Image --
   -----------------

   function Value_Image
     (Expression : in Asis.Expression)
      return Wide_String
   is
   begin
      Check_Nil_Element (Expression, "Value_Image");
      return Get_Image (Expression, P.Value_Image);
   end Value_Image;

end Asis.Expressions;


------------------------------------------------------------------------------
--  Copyright (c) 2006, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
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
