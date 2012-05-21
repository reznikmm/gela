------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-elements.adb 2654 2008-08-15 17:07:37Z maxr $

--with XASIS.Utils;

with Asis.Gela.To;
with Asis.Gela.Strings;
with Asis.Gela.Contexts;
with Asis.Gela.Elements;
with Asis.Gela.Base_Lists;
with Asis.Gela.Compilations;

with Gela; use Gela;
with Gela.Decoders;
with Gela.Source_Buffers;

package body Asis.Elements is
   use Asis.Gela;
   use Asis.Gela.Elements;
   use Asis.Gela.Contexts;

   ----------------------------
   -- Access_Definition_Kind --
   ----------------------------

   function Access_Definition_Kind
     (Definition : Asis.Definition)
      return Asis.Access_Definition_Kinds
   is
   begin
      if Is_Nil (Definition) then
         return Not_An_Access_Definition;
      else
         return To.Access_Definition_Kinds
           (Global_Kind (Get_Compilation (Definition.Unit), Definition.Index));
      end if;
   end Access_Definition_Kind;

   ----------------------
   -- Access_Type_Kind --
   ----------------------

   function Access_Type_Kind
     (Definition : in Asis.Access_Type_Definition)
      return Asis.Access_Type_Kinds
   is
   begin
      if Is_Nil (Definition) then
         return Not_An_Access_Type_Definition;
      else
         return To.Access_Type_Kinds
           (Global_Kind (Get_Compilation (Definition.Unit), Definition.Index));
      end if;
   end Access_Type_Kind;

   ----------------------
   -- Association_Kind --
   ----------------------

   function Association_Kind
     (Association : in Asis.Association)
      return Asis.Association_Kinds
   is
   begin
      if Is_Nil (Association) then
         return Not_An_Association;
      else
         return To.Association_Kinds
           (Global_Kind (Get_Compilation (Association.Unit),
                         Association.Index));
      end if;
   end Association_Kind;

   --------------------
   -- Attribute_Kind --
   --------------------

   function Attribute_Kind
     (Expression : in Asis.Expression)
      return Asis.Attribute_Kinds
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Expression) then
         Get (Get_Compilation (Expression.Unit),
              Expression.Index,
              P.Attribute_Kind,
              Result,
              Success);

         if Success then
            return Attribute_Kinds'Val (Result);
         end if;
      end if;

      return Not_An_Attribute;
   end Attribute_Kind;

   -----------------
   -- Clause_Kind --
   -----------------

   function Clause_Kind
     (Clause : in Asis.Clause)
      return Asis.Clause_Kinds
   is
   begin
      if Is_Nil (Clause) then
         return Not_A_Clause;
      else
         return To.Clause_Kinds
           (Global_Kind (Get_Compilation (Clause.Unit), Clause.Index));
      end if;
   end Clause_Kind;

   -------------------------
   -- Compilation_Pragmas --
   -------------------------

   function Compilation_Pragmas
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Pragma_Element_List
   is
   begin
      Check_Nil_Unit (Compilation_Unit, "Compilation_Pragmas");
      raise Not_Implemented_Error;
      return Asis.Nil_Element_List;
   end Compilation_Pragmas;

   ---------------------------
   -- Configuration_Pragmas --
   ---------------------------

   function Configuration_Pragmas
     (The_Context : in Asis.Context)
      return Asis.Pragma_Element_List
   is
   begin
      Check_Context (The_Context);
      raise Not_Implemented_Error;
      return Asis.Nil_Element_List;
   end Configuration_Pragmas;

   ---------------------
   -- Constraint_Kind --
   ---------------------

   function Constraint_Kind
     (Definition : in Asis.Constraint)
      return Asis.Constraint_Kinds
   is
   begin
      if Is_Nil (Definition) then
         return Not_A_Constraint;
      else
         return To.Constraint_Kinds
           (Global_Kind (Get_Compilation (Definition.Unit), Definition.Index));
      end if;
   end Constraint_Kind;

   -----------------------------
   -- Context_Clause_Elements --
   -----------------------------

   function Context_Clause_Elements
     (Compilation_Unit : in Asis.Compilation_Unit;
      Include_Pragmas  : in Boolean := False)
      return Asis.Context_Clause_List
   is
   begin
      Check_Nil_Unit (Compilation_Unit, "Context_Clause_Elements");

      declare
         Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

         C : constant Compilations.Compilation :=
           Get_Compilation (Compilation_Unit);

         List : constant Element_Index :=
           Get (C, Unit, P.Context_Clause_Elements);
      begin
         return Base_Lists.To_Element_List
           (C, List, Compilation_Unit, Include_Pragmas);
      end;

   end Context_Clause_Elements;

   ---------------------------
   -- Corresponding_Pragmas --
   ---------------------------

   function Corresponding_Pragmas
     (Element : in Asis.Element)
      return Asis.Pragma_Element_List
   is
   begin
      Check_Nil_Element (Element, "Corresponding_Pragmas");
      raise Not_Implemented_Error;
      return Asis.Nil_Element_List;
   end Corresponding_Pragmas;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (Element : in Asis.Element) return Wide_String is
   begin
      return "";
      -- renames XASIS.Utils.Debug_Image;
   end Debug_Image;

   ----------------------
   -- Declaration_Kind --
   ----------------------

   function Declaration_Kind
     (Declaration : in Asis.Declaration)
      return Asis.Declaration_Kinds
   is
   begin
      if Is_Nil (Declaration) then
         return Not_A_Declaration;
      else
         return To.Declaration_Kinds
           (Global_Kind (Get_Compilation (Declaration.Unit),
                         Declaration.Index));
      end if;
   end Declaration_Kind;

   ------------------------
   -- Declaration_Origin --
   ------------------------

   function Declaration_Origin
     (Declaration : in Asis.Declaration)
      return Asis.Declaration_Origins
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Declaration) then
         Get (Get_Compilation (Declaration.Unit),
              Declaration.Index,
              P.Declaration_Origin,
              Result,
              Success);

         if Success then
            return Declaration_Origins'Val (Result);
         end if;
      end if;

      return Not_A_Declaration_Origin;
   end Declaration_Origin;

   ------------------
   -- Default_Kind --
   ------------------

   function Default_Kind
     (Declaration : in Asis.Generic_Formal_Parameter)
      return Asis.Subprogram_Default_Kinds
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Declaration) then
         Get (Get_Compilation (Declaration.Unit),
              Declaration.Index,
              P.Default_Kind,
              Result,
              Success);

         if Success then
            return Subprogram_Default_Kinds'Val (Result);
         end if;
      end if;

      return Not_A_Default;
   end Default_Kind;

   ------------------------
   -- Defining_Name_Kind --
   ------------------------

   function Defining_Name_Kind
     (Defining_Name : in Asis.Defining_Name)
      return Asis.Defining_Name_Kinds
   is
   begin
      if Is_Nil (Defining_Name) then
         return Not_A_Defining_Name;
      else
         return To.Defining_Name_Kinds
           (Global_Kind (Get_Compilation (Defining_Name.Unit),
                         Defining_Name.Index));
      end if;
   end Defining_Name_Kind;

   ---------------------
   -- Definition_Kind --
   ---------------------

   function Definition_Kind
     (Definition : in Asis.Definition)
      return Asis.Definition_Kinds
   is
   begin
      if Is_Nil (Definition) then
         return Not_A_Definition;
      else
         return To.Definition_Kinds
           (Global_Kind (Get_Compilation (Definition.Unit), Definition.Index));
      end if;
   end Definition_Kind;

   -------------------------
   -- Discrete_Range_Kind --
   -------------------------

   function Discrete_Range_Kind
     (Definition : in Asis.Discrete_Range)
      return Asis.Discrete_Range_Kinds
   is
   begin
      if Is_Nil (Definition) then
         return Not_A_Discrete_Range;
      else
         return To.Discrete_Range_Kinds
           (Global_Kind (Get_Compilation (Definition.Unit), Definition.Index));
      end if;
   end Discrete_Range_Kind;

   ------------------
   -- Element_Kind --
   ------------------

   function Element_Kind
     (Element : in Asis.Element)
      return Asis.Element_Kinds
   is
   begin
      if Is_Nil (Element) then
         return Not_An_Element;
      else
         return To.Element_Kinds
           (Global_Kind (Get_Compilation (Element.Unit), Element.Index));
      end if;
   end Element_Kind;

   --------------------------------
   -- Enclosing_Compilation_Unit --
   --------------------------------

   function Enclosing_Compilation_Unit
     (Element : in Asis.Element)
      return Asis.Compilation_Unit
   is
   begin
      Check_Nil_Element (Element, "Enclosing_Compilation_Unit");
      return Element.Unit;
   end Enclosing_Compilation_Unit;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   function Enclosing_Element
     (Element : in Asis.Element)
      return Asis.Element
   is
   begin
      Check_Nil_Element (Element, "Enclosing_Element");
      return (Unit  => Element.Unit,
              Index => Get (Get_Compilation (Element.Unit),
                            Element.Index,
                            P.Enclosing_Element));
   end Enclosing_Element;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   function Enclosing_Element
     (Element                    : in Asis.Element;
      Expected_Enclosing_Element : in Asis.Element)
      return Asis.Element
   is
   begin
      return Enclosing_Element (Element);
   end Enclosing_Element;

   ---------------------
   -- Expression_Kind --
   ---------------------

   function Expression_Kind
     (Expression : in Asis.Expression)
      return Asis.Expression_Kinds
   is
   begin
      if Is_Nil (Expression) then
         return Not_An_Expression;
      else
         return To.Expression_Kinds
           (Global_Kind (Get_Compilation (Expression.Unit), Expression.Index));
      end if;
   end Expression_Kind;

   ----------------------
   -- Formal_Type_Kind --
   ----------------------

   function Formal_Type_Kind
     (Definition : in Asis.Formal_Type_Definition)
      return Asis.Formal_Type_Kinds
   is
   begin
      if Is_Nil (Definition) then
         return Not_A_Formal_Type_Definition;
      else
         return To.Formal_Type_Kinds
           (Global_Kind (Get_Compilation (Definition.Unit), Definition.Index));
      end if;
   end Formal_Type_Kind;

   -----------------
   -- Has_Limited --
   -----------------

   function Has_Limited (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Limited);
   end Has_Limited;

   -----------------
   -- Has_Private --
   -----------------

   function Has_Private (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Private);
   end Has_Private;

   ------------------
   -- Has_Abstract --
   ------------------

   function Has_Abstract (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Abstract);
   end Has_Abstract;

   -----------------
   -- Has_Reverse --
   -----------------

   function Has_Reverse (Element : in Asis.Element) return Boolean is
   begin
      return Trait_Kind (Element) = A_Reverse_Trait;
   end Has_Reverse;

   -----------------
   -- Has_Aliased --
   -----------------

   function Has_Aliased (Element : in Asis.Element) return Boolean is
   begin
      return Trait_Kind (Element) = An_Aliased_Trait;
   end Has_Aliased;

   ----------------------
   -- Has_Synchronized --
   ----------------------

   function Has_Synchronized (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Synchronized);
   end Has_Synchronized;

   -------------------
   -- Has_Protected --
   -------------------

   function Has_Protected (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Protected);
   end Has_Protected;

   ----------------
   -- Has_Tagged --
   ----------------

   function Has_Tagged (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Tagged);
   end Has_Tagged;

   --------------
   -- Has_Task --
   --------------

   function Has_Task (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Task);
   end Has_Task;

   ------------------------
   -- Has_Null_Exclusion --
   ------------------------

   function Has_Null_Exclusion (Element : Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Has_Null_Exclusion);
   end Has_Null_Exclusion;

   ----------
   -- Hash --
   ----------

   function Hash (Element : in Asis.Element) return Asis.ASIS_Integer is
   begin
      return Element.Index;
   end Hash;

   --------------------
   -- Interface_Kind --
   --------------------

   function Interface_Kind
     (Definition : Asis.Definition)
     return Asis.Interface_Kinds is
   begin
      if not Is_Nil (Definition) then
         if Type_Kind (Definition) = An_Interface_Type_Definition or
           Formal_Type_Kind (Definition) =
           A_Formal_Interface_Type_Definition
         then
            if Has_Task (Definition) then
               return A_Task_Interface;
            elsif Has_Limited (Definition) then
               return A_Limited_Interface;
            elsif Has_Protected (Definition) then
               return A_Protected_Interface;
            elsif Has_Synchronized (Definition) then
               return A_Synchronized_Interface;
            else
               return An_Ordinary_Interface;
            end if;
         end if;
      end if;

      return Not_An_Interface;
   end Interface_Kind;

   ----------------------------
   -- Is_Abstract_Subprogram --
   ----------------------------

   function Is_Abstract_Subprogram
     (Element : in Asis.Element)
      return Boolean
   is
   begin
      case Declaration_Kind (Element) is
         when A_Procedure_Declaration |
           A_Function_Declaration |
           A_Formal_Procedure_Declaration |
           A_Formal_Function_Declaration =>
            return Has_Abstract (Element);
         when others =>
            return False;
      end case;
   end Is_Abstract_Subprogram;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : in Asis.Element;
      Right : in Asis.Element)
     return Boolean renames Is_Identical;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : in Asis.Element;
      Right : in Asis.Element)
      return Boolean is
   begin
      return Left.Unit.Index = Right.Unit.Index and
        Left.Index = Right.Index;
   end Is_Identical;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Asis.Element) return Boolean is
   begin
      return not Assigned (Right);
   end Is_Nil;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Asis.Element_List) return Boolean is
   begin
      return Right'Length = 0;
   end Is_Nil;

   -----------------------
   -- Is_Null_Procedure --
   -----------------------

   function Is_Null_Procedure (Element : in Asis.Element) return Boolean is
      use type P.Global_Kinds;
   begin
      if Is_Nil (Element) then
         return False;
      elsif Declaration_Kind (Element) = A_Formal_Procedure_Declaration then
         declare
            C : constant Compilations.Compilation :=
              Get_Compilation (Element.Unit);

            Default : constant Element_Index :=
              Get (C, Element.Index, P.Formal_Subprogram_Default);
         begin
            return Global_Kind (C, Default) = P.A_Null_Literal;
         end;
      else
         raise Not_Implemented_Error;
         return Is_Null_Procedure (Element);
      end if;
   end Is_Null_Procedure;

   -------------------------
   -- Is_Part_Of_Implicit --
   -------------------------

   function Is_Part_Of_Implicit (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Is_Part_Of_Implicit);
   end Is_Part_Of_Implicit;

   --------------------------
   -- Is_Part_Of_Inherited --
   --------------------------

   function Is_Part_Of_Inherited
     (Element : in Asis.Element)
      return Boolean
   is
   begin
      return Get_Boolean (Element, P.Is_Part_Of_Inherited);
   end Is_Part_Of_Inherited;

   -------------------------
   -- Is_Part_Of_Instance --
   -------------------------

   function Is_Part_Of_Instance (Element : in Asis.Element) return Boolean is
   begin
      return Get_Boolean (Element, P.Is_Part_Of_Instance);
   end Is_Part_Of_Instance;

   ---------------
   -- Mode_Kind --
   ---------------

   function Mode_Kind
     (Declaration : in Asis.Declaration)
      return Asis.Mode_Kinds
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Declaration) then
         Get (Get_Compilation (Declaration.Unit),
              Declaration.Index,
              P.Mode_Kind,
              Result,
              Success);

         if Success then
            return Mode_Kinds'Val (Result);
         end if;
      end if;

      return Not_A_Mode;
   end Mode_Kind;

   -------------------
   -- Operator_Kind --
   -------------------

   function Operator_Kind
     (Element : in Asis.Element)
      return Asis.Operator_Kinds
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Element) then
         Get (Get_Compilation (Element.Unit),
              Element.Index,
              P.Operator_Kind,
              Result,
              Success);

         if Success then
            return Operator_Kinds'Val (Result);
         end if;
      end if;

      return Not_An_Operator;
   end Operator_Kind;

   ---------------
   -- Path_Kind --
   ---------------

   function Path_Kind (Path : in Asis.Path) return Asis.Path_Kinds is
   begin
      if Is_Nil (Path) then
         return Not_A_Path;
      else
         return To.Path_Kinds
           (Global_Kind (Get_Compilation (Path.Unit), Path.Index));
      end if;
   end Path_Kind;

   ----------------------------------
   -- Pragma_Argument_Associations --
   ----------------------------------

   function Pragma_Argument_Associations
     (Pragma_Element : in Asis.Pragma_Element)
      return Asis.Association_List
   is
   begin
      Check_Nil_Element (Pragma_Element, "Pragma_Argument_Associations");
      raise Not_Implemented_Error;
      return Asis.Nil_Element_List;
   end Pragma_Argument_Associations;

   -----------------
   -- Pragma_Kind --
   -----------------

   function Pragma_Kind
     (Pragma_Element : in Asis.Pragma_Element)
      return Asis.Pragma_Kinds
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Pragma_Element) then
         Get (Get_Compilation (Pragma_Element.Unit),
              Pragma_Element.Index,
              P.Pragma_Kind,
              Result,
              Success);

         if Success then
            return Pragma_Kinds'Val (Result);
         end if;
      end if;

      return Not_A_Pragma;
   end Pragma_Kind;

   -----------------------
   -- Pragma_Name_Image --
   -----------------------

   function Pragma_Name_Image
     (Pragma_Element : in Asis.Pragma_Element)
      return Program_Text
   is
   begin
      Check_Nil_Element (Pragma_Element, "Pragma_Name_Image");
      return Get_Image (Pragma_Element, P.Pragma_Name_Image);
   end Pragma_Name_Image;

   -------------
   -- Pragmas --
   -------------

   function Pragmas
     (The_Element : in Asis.Element)
      return Asis.Pragma_Element_List
   is
   begin
      Check_Nil_Element (The_Element, "Pragmas");
      raise Not_Implemented_Error;
      return Asis.Nil_Element_List;
   end Pragmas;

   --------------------------------
   -- Representation_Clause_Kind --
   --------------------------------

   function Representation_Clause_Kind
     (Clause : in Asis.Representation_Clause)
      return Asis.Representation_Clause_Kinds
   is
   begin
      if Is_Nil (Clause) then
         return Not_A_Representation_Clause;
      else
         return To.Representation_Clause_Kinds
           (Global_Kind (Get_Compilation (Clause.Unit), Clause.Index));
      end if;
   end Representation_Clause_Kind;

   --------------------
   -- Root_Type_Kind --
   --------------------

   function Root_Type_Kind
     (Definition : in Asis.Root_Type_Definition)
      return Asis.Root_Type_Kinds
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Definition) then
         Get (Get_Compilation (Definition.Unit),
              Definition.Index,
              P.Root_Type_Kind,
              Result,
              Success);

         if Success then
            return Root_Type_Kinds'Val (Result);
         end if;
      end if;

      return Not_A_Root_Type_Definition;
   end Root_Type_Kind;

   --------------------
   -- Statement_Kind --
   --------------------

   function Statement_Kind
     (Statement : in Asis.Statement)
      return Asis.Statement_Kinds
   is
   begin
      if Is_Nil (Statement) then
         return Not_A_Statement;
      else
         return To.Statement_Kinds
           (Global_Kind (Get_Compilation (Statement.Unit), Statement.Index));
      end if;
   end Statement_Kind;

   ----------------
   -- Trait_Kind --
   ----------------

   function Trait_Kind
     (Element : in Asis.Element)
      return Asis.Trait_Kinds
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if not Is_Nil (Element) then
         Get (Get_Compilation (Element.Unit),
              Element.Index,
              P.Trait_Kind,
              Result,
              Success);

         if Success then
            return Trait_Kinds'Val (Result);
         end if;
      end if;

      return Not_A_Trait;
   end Trait_Kind;

   ---------------
   -- Type_Kind --
   ---------------

   function Type_Kind
     (Definition : in Asis.Type_Definition)
      return Asis.Type_Kinds
   is
   begin
      if Is_Nil (Definition) then
         return Not_A_Type_Definition;
      else
         return To.Type_Kinds
           (Global_Kind (Get_Compilation (Definition.Unit), Definition.Index));
      end if;
   end Type_Kind;

   ----------------------
   -- Unit_Declaration --
   ----------------------

   function Unit_Declaration
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Declaration
   is
   begin
      Check_Nil_Unit (Compilation_Unit, "Unit_Declaration");

      declare
         Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

         C : constant Compilations.Compilation :=
           Get_Compilation (Compilation_Unit);

         Result : constant Element_Index :=
           Get (C, Unit, P.Unit_Declaration);
      begin
         return (Unit  => Compilation_Unit,
                 Index => Result);
      end;
   end Unit_Declaration;

end Asis.Elements;


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
