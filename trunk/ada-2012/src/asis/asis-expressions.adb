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

with Gela.Compilations;
with Gela.Element_Visiters;
with Gela.Elements.Association_Lists;
with Gela.Elements.Associations;
with Gela.Elements.Auxiliary_Applies;
with Gela.Elements.Character_Literals;
with Gela.Elements.Defining_Names;
with Gela.Elements.Expression_Or_Boxes;
with Gela.Elements.Identifiers;
with Gela.Elements.Numeric_Literals;
with Gela.Elements.Operator_Symbols;
with Gela.Elements.Prefixes;
with Gela.Elements.Selected_Components;
with Gela.Elements.Selected_Identifiers;
with Gela.Elements.Selector_Names;
with Gela.Elements.String_Literals;
with Gela.Lexical_Types;

package body Asis.Expressions is

   ----------------------
   -- Actual_Parameter --
   ----------------------

   function Actual_Parameter
     (Association : in Asis.Association)
      return Asis.Expression
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Expression_Or_Boxes.
                       Expression_Or_Box_Access;
         end record;

         overriding procedure Association
           (Self : in out Visiter;
            Node : not null Gela.Elements.Associations.Association_Access);

      end Get;

      package body Get is

         -----------------
         -- Association --
         -----------------

         overriding procedure Association
           (Self : in out Visiter;
            Node : not null Gela.Elements.Associations.Association_Access) is
         begin
            Self.Result := Node.Component_Expression;
         end Association;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Association, "Actual_Parameter");
      Association.Data.Visit (V);
      return (Data => Gela.Elements.Element_Access (V.Result));
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Corresponding_Name_Declaration;

   -----------------------------------
   -- Corresponding_Name_Definition --
   -----------------------------------

   function Corresponding_Name_Definition
     (Reference : in Asis.Expression)
      return Asis.Defining_Name
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Defining_Names.Defining_Name_Access;
         end record;

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access);

         overriding procedure Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Operator_Symbols.
              Operator_Symbol_Access);

      end Get;

      package body Get is

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access)
         is
         begin
            Self.Result := Node.Defining_Name;
         end Identifier;

         overriding procedure Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Operator_Symbols.
              Operator_Symbol_Access) is
         begin
            Self.Result := Node.Defining_Name;
         end Operator_Symbol;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Reference, "Corresponding_Name_Definition");
      Reference.Data.Visit (V);
      return (Data => Gela.Elements.Element_Access (V.Result));
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Formal_Parameter;

   ------------------------------
   -- Function_Call_Parameters --
   ------------------------------

   function Function_Call_Parameters
     (Expression : in Asis.Expression;
      Normalized : in Boolean := False)
      return Asis.Association_List
   is
      pragma Unreferenced (Normalized);

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Sequence_Access;
         end record;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access);
      end Get;

      package body Get is

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access)
         is
            Agg : constant Gela.Elements.Association_Lists.
              Association_List_Access :=
                Node.Function_Call_Parameters;
            Seq : constant Gela.Elements.Associations.
              Association_Sequence_Access :=
                Agg.Record_Component_Associations;
         begin
            Self.Result := Gela.Elements.Element_Sequence_Access (Seq);
         end Auxiliary_Apply;

      end Get;

      V       : Get.Visiter;
   begin
      Check_Nil_Element (Expression, "Function_Call_Parameters");
      Expression.Data.Visit (V);
      return Asis.To_List (V.Result);
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      Raise_Not_Implemented ("");
      return False;
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
      Raise_Not_Implemented ("");
      return False;
   end Is_Normalized;

   --------------------
   -- Is_Prefix_Call --
   --------------------

   function Is_Prefix_Call
     (Expression : in Asis.Expression)
      return Boolean
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Boolean := False;
         end record;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access);

         overriding procedure Association_List
           (Self : in out Visiter;
            Node : not null Gela.Elements.Association_Lists.
              Association_List_Access);
      end Get;

      package body Get is

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access)
         is
            Args : constant Gela.Elements.Association_Lists.
              Association_List_Access := Node.Function_Call_Parameters;
         begin
            if Args.Assigned then
               Args.Visit (Self);
            else
               Self.Result := True;
            end if;
         end Auxiliary_Apply;

         overriding procedure Association_List
           (Self : in out Visiter;
            Node : not null Gela.Elements.Association_Lists.
              Association_List_Access)
         is
            use type Gela.Lexical_Types.Token_Count;
         begin
            Self.Result := Node.Left_Token /= 0;
         end Association_List;
      end Get;

      V       : Get.Visiter;
   begin
      Check_Nil_Element (Expression, "Is_Prefix_Call");
      Expression.Data.Visit (V);
      return V.Result;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Membership_Test_Subtype_Mark;

   ----------------
   -- Name_Image --
   ----------------

   function Name_Image
     (Expression : in Asis.Expression)
      return Program_Text
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Symbol : Gela.Lexical_Types.Symbol := 0;
         end record;

         overriding procedure Character_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Character_Literals.
              Character_Literal_Access);

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access);

         overriding procedure Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Operator_Symbols.
              Operator_Symbol_Access);
      end Get;

      package body Get is

         overriding procedure Character_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Character_Literals.
              Character_Literal_Access)
         is
            Token : constant Gela.Lexical_Types.Token_Count :=
              Node.Character_Literal_Token;
            Comp  : constant Gela.Compilations.Compilation_Access :=
              Node.Enclosing_Compilation;
         begin
            Self.Symbol := Comp.Get_Token (Token).Symbol;
         end Character_Literal;

         ----------------
         -- Identifier --
         ----------------

         overriding procedure Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Identifiers.Identifier_Access)
         is
            Token : constant Gela.Lexical_Types.Token_Count :=
              Node.Identifier_Token;
            Comp  : constant Gela.Compilations.Compilation_Access :=
              Node.Enclosing_Compilation;
         begin
            Self.Symbol := Comp.Get_Token (Token).Symbol;
         end Identifier;

         overriding procedure Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Operator_Symbols.
              Operator_Symbol_Access)
         is
            Token : constant Gela.Lexical_Types.Token_Count :=
              Node.Operator_Symbol_Token;
            Comp  : constant Gela.Compilations.Compilation_Access :=
              Node.Enclosing_Compilation;
         begin
            Self.Symbol := Comp.Get_Token (Token).Symbol;
         end Operator_Symbol;

      end Get;

      use type Gela.Lexical_Types.Symbol;

      V       : Get.Visiter;
      Comp    : Gela.Compilations.Compilation_Access;
      Context : Gela.Contexts.Context_Access;
   begin
      Check_Nil_Element (Expression, "Name_Image");
      Expression.Data.Visit (V);
      Assert_Inappropriate_Element (V.Symbol /= 0, "Name_Image");
      Comp := Expression.Data.Enclosing_Compilation;
      Context := Comp.Context;
      return Context.Symbols.Image (V.Symbol).To_UTF_16_Wide_String;
   end Name_Image;

   ------------
   -- Prefix --
   ------------

   function Prefix
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access);
      end Get;

      package body Get is

         overriding procedure Auxiliary_Apply
           (Self : in out Visiter;
            Node : not null Gela.Elements.Auxiliary_Applies.
              Auxiliary_Apply_Access)
         is
            Prefix : constant Gela.Elements.Prefixes.Prefix_Access :=
              Node.Prefix;
         begin
            Self.Result := Gela.Elements.Element_Access (Prefix);
         end Auxiliary_Apply;

      end Get;

      V       : Get.Visiter;
   begin
      Check_Nil_Element (Expression, "Prefix");
      Expression.Data.Visit (V);
      return (Data => V.Result);
   end Prefix;

   -----------------------------------
   -- Record_Component_Associations --
   -----------------------------------

   function Record_Component_Associations
     (Expression : in Asis.Expression;
      Normalized : in Boolean := False)
      return Asis.Association_List
   is
      pragma Unreferenced (Normalized);
   begin
      Check_Nil_Element (Expression, "Record_Component_Associations");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
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
      pragma Unreferenced (Implicitly);
      pragma Unreferenced (Within_Element);
   begin
      Check_Nil_Element (Name, "References");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end References;

   --------------
   -- Selector --
   --------------

   function Selector
     (Expression : in Asis.Expression)
      return Asis.Expression
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Found    : Boolean := False;
            Selector : Gela.Elements.Selector_Names.Selector_Name_Access;
         end record;

         overriding procedure Selected_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Components.
              Selected_Component_Access);

         overriding procedure Selected_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Identifiers.
              Selected_Identifier_Access);
      end Get;

      package body Get is

         ------------------------
         -- Selected_Component --
         ------------------------

         overriding procedure Selected_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Components.
              Selected_Component_Access) is
         begin
            Self.Found := True;
            Self.Selector := Node.Selector;
         end Selected_Component;

         -------------------------
         -- Selected_Identifier --
         -------------------------

         overriding procedure Selected_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selected_Identifiers.
              Selected_Identifier_Access) is
         begin
            Self.Found := True;
            Self.Selector := Node.Selector;
         end Selected_Identifier;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Expression, "Selector");
      Expression.Data.Visit (V);
      Assert_Inappropriate_Element (V.Found, "Selector");
      return (Data => Gela.Elements.Element_Access (V.Selector));
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Slice_Range;

   -----------------
   -- Value_Image --
   -----------------

   function Value_Image
     (Expression : in Asis.Expression)
      return Wide_String
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Token : Gela.Lexical_Types.Token_Count;
         end record;

         overriding procedure Numeric_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Numeric_Literals.
              Numeric_Literal_Access);

         overriding procedure String_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.String_Literals.
              String_Literal_Access);
      end Get;

      package body Get is

         overriding procedure Numeric_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Numeric_Literals.
              Numeric_Literal_Access)
         is
         begin
            Self.Token := Node.Numeric_Literal_Token;
         end Numeric_Literal;

         overriding procedure String_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.String_Literals.
              String_Literal_Access)
         is
         begin
            Self.Token := Node.String_Literal_Token;
         end String_Literal;

      end Get;

      V       : Get.Visiter;
      Comp    : Gela.Compilations.Compilation_Access;
      Source  : League.Strings.Universal_String;
      Token   : Gela.Lexical_Types.Token;
   begin
      Check_Nil_Element (Expression, "Value_Image");
      Expression.Data.Visit (V);
      Comp := Expression.Data.Enclosing_Compilation;
      Source := Comp.Source;
      Token := Comp.Get_Token (V.Token);
      return Source.Slice (Token.First, Token.Last).To_UTF_16_Wide_String;
   end Value_Image;

end Asis.Expressions;


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
