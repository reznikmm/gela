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

with Gela.Elements.Compilation_Units;
with Gela.Element_Visiters;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Subunits;
with Gela.Elements.Context_Items;
with Asis.Extensions.Flat_Kinds;
with Gela.Elements.Library_Unit_Bodies;
with Gela.Elements.Library_Unit_Declarations;
with Gela.Elements.Proper_Bodies;

package body Asis.Elements is

   package F renames Asis.Extensions.Flat_Kinds;

   ----------------------------
   -- Access_Definition_Kind --
   ----------------------------

   function Access_Definition_Kind
     (Definition : Asis.Definition)
      return Asis.Access_Definition_Kinds
   is
   begin
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_An_Access_Definition;
      else
         return Not_An_Access_Definition;
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
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_An_Access_Type_Definition;
      else
         return Not_An_Access_Type_Definition;
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
      if Assigned (Association) then
         Raise_Not_Implemented ("");
         return Not_An_Association;
      else
         return Not_An_Association;
      end if;
   end Association_Kind;

   --------------------
   -- Attribute_Kind --
   --------------------

   function Attribute_Kind
     (Expression : in Asis.Expression)
      return Asis.Attribute_Kinds
   is
   begin
      if Assigned (Expression) then
         Raise_Not_Implemented ("");
         return Not_An_Attribute;
      else
         return Not_An_Attribute;
      end if;
   end Attribute_Kind;

   -----------------
   -- Clause_Kind --
   -----------------

   function Clause_Kind (Clause : in Asis.Clause) return Asis.Clause_Kinds is
   begin
      case F.Flat_Kind (Clause) is
         when F.A_Use_Package_Clause =>
            return A_Use_Package_Clause;
         when F.A_Use_Type_Clause =>
            return A_Use_Type_Clause;
         when F.A_With_Clause =>
            return A_With_Clause;
         when F.A_Representation_Clause =>
            return A_Representation_Clause;
         when F.A_Component_Clause =>
            return A_Component_Clause;
         when others =>
            return Not_A_Clause;
      end case;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Compilation_Pragmas;

   ---------------------------
   -- Configuration_Pragmas --
   ---------------------------

   function Configuration_Pragmas
     (The_Context : in Asis.Context)
      return Asis.Pragma_Element_List
   is
      pragma Unreferenced (The_Context);
   begin
      --  Check_Context (The_Context);
      Raise_Not_Implemented ("");
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
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Constraint;
      else
         return Not_A_Constraint;
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
      pragma Unreferenced (Include_Pragmas);

      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Context_Items.Context_Item_Sequence_Access;
         end record;

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access);

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access);

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access);

      end Get;

      package body Get is

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access) is
         begin
            Self.Result := Node.Context_Clause_Elements;
         end Compilation_Unit_Body;

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access) is
         begin
            Self.Result := Node.Context_Clause_Elements;
         end Compilation_Unit_Declaration;

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access) is
         begin
            Self.Result := Node.Context_Clause_Elements;
         end Subunit;

      end Get;

      Tree : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
      V : aliased Get.Visiter;
   begin
      Check_Nil_Unit (Compilation_Unit, "Context_Clause_Elements");
      Tree := Compilation_Unit.Data.Tree;
      Tree.Visit (V);

      declare
         Result : Asis.Element_List (1 .. Asis.ASIS_Natural (V.Result.Length));
         C : Gela.Elements.Context_Items.Context_Item_Sequence_Cursor :=
           V.Result.First;
      begin
         for J in Result'Range loop
            Result (J) := (Data => Gela.Elements.Element_Access (C.Element));
            C.Next;
         end loop;

         return Result;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Corresponding_Pragmas;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (Element : in Asis.Element) return Wide_String is
      pragma Unreferenced (Element);
   begin
      return "";
   end Debug_Image;

   ----------------------
   -- Declaration_Kind --
   ----------------------

   function Declaration_Kind
     (Declaration : in Asis.Declaration)
      return Asis.Declaration_Kinds
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_A_Declaration;
      else
         return Not_A_Declaration;
      end if;
   end Declaration_Kind;

   ------------------------
   -- Declaration_Origin --
   ------------------------

   function Declaration_Origin
     (Declaration : in Asis.Declaration)
      return Asis.Declaration_Origins
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_A_Declaration_Origin;
      else
         return Not_A_Declaration_Origin;
      end if;
   end Declaration_Origin;

   ------------------
   -- Default_Kind --
   ------------------

   function Default_Kind
     (Declaration : in Asis.Generic_Formal_Parameter)
      return Asis.Subprogram_Default_Kinds
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_A_Default;
      else
         return Not_A_Default;
      end if;
   end Default_Kind;

   ------------------------
   -- Defining_Name_Kind --
   ------------------------

   function Defining_Name_Kind
     (Defining_Name : in Asis.Defining_Name)
      return Asis.Defining_Name_Kinds
   is
   begin
      if Assigned (Defining_Name) then
         Raise_Not_Implemented ("");
         return Not_A_Defining_Name;
      else
         return Not_A_Defining_Name;
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
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Definition;
      else
         return Not_A_Definition;
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
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Discrete_Range;
      else
         return Not_A_Discrete_Range;
      end if;
   end Discrete_Range_Kind;

   ------------------
   -- Element_Kind --
   ------------------

   function Element_Kind
     (Element : in Asis.Element) return Asis.Element_Kinds is
   begin
      case F.Flat_Kind (Element) is
         when F.A_Pragma =>
            return A_Pragma;
         when F.A_Defining_Name =>
            return A_Defining_Name;
         when F.A_Declaration =>
            return A_Declaration;
         when F.A_Definition =>
            return A_Definition;
         when F.An_Expression =>
            return An_Expression;
         when F.An_Association =>
            return An_Association;
         when F.A_Statement =>
            return A_Statement;
         when F.A_Path =>
            return A_Path;
         when F.A_Clause =>
            return A_Clause;
         when F.An_Exception_Handler =>
            return An_Exception_Handler;
         when others =>
            return Not_An_Element;
      end case;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Compilation_Unit;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element;
   end Enclosing_Element;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   function Enclosing_Element
     (Element                    : in Asis.Element;
      Expected_Enclosing_Element : in Asis.Element)
      return Asis.Element
   is
      pragma Unreferenced (Expected_Enclosing_Element);
   begin
      return Enclosing_Element (Element);
   end Enclosing_Element;

   ---------------------
   -- Expression_Kind --
   ---------------------

   function Expression_Kind
     (Expression : in Asis.Expression) return Asis.Expression_Kinds is
   begin
      case F.Flat_Kind (Expression) is
         when F.A_Box_Expression =>
            return Asis.A_Box_Expression;
         when F.An_Integer_Literal =>
            return Asis.An_Integer_Literal;
         when F.A_Real_Literal =>
            return Asis.A_Real_Literal;
         when F.A_String_Literal =>
            return Asis.A_String_Literal;
         when F.An_Identifier =>
            return Asis.An_Identifier;
         when F.An_Operator_Symbol =>
            return Asis.An_Operator_Symbol;
         when F.A_Character_Literal =>
            return Asis.A_Character_Literal;
         when F.An_Enumeration_Literal =>
            return Asis.An_Enumeration_Literal;
         when F.An_Explicit_Dereference =>
            return Asis.An_Explicit_Dereference;
         when F.A_Function_Call =>
            return Asis.A_Function_Call;
         when F.An_Indexed_Component =>
            return Asis.An_Indexed_Component;
         when F.A_Slice =>
            return Asis.A_Slice;
         when F.A_Selected_Component =>
            return Asis.A_Selected_Component;
         when F.An_Attribute_Reference =>
            return Asis.An_Attribute_Reference;
         when F.A_Record_Aggregate =>
            return Asis.A_Record_Aggregate;
         when F.An_Extension_Aggregate =>
            return Asis.An_Extension_Aggregate;
         when F.A_Positional_Array_Aggregate =>
            return Asis.A_Positional_Array_Aggregate;
         when F.A_Named_Array_Aggregate =>
            return Asis.A_Named_Array_Aggregate;
         when F.An_And_Then_Short_Circuit =>
            return Asis.An_And_Then_Short_Circuit;
         when F.An_Or_Else_Short_Circuit =>
            return Asis.An_Or_Else_Short_Circuit;
         when F.An_In_Range_Membership_Test =>
            return Asis.An_In_Range_Membership_Test;
         when F.A_Not_In_Range_Membership_Test =>
            return Asis.A_Not_In_Range_Membership_Test;
         when F.An_In_Type_Membership_Test =>
            return Asis.An_In_Type_Membership_Test;
         when F.A_Not_In_Type_Membership_Test =>
            return Asis.A_Not_In_Type_Membership_Test;
         when F.A_Null_Literal =>
            return Asis.A_Null_Literal;
         when F.A_Parenthesized_Expression =>
            return Asis.A_Parenthesized_Expression;
         when F.A_Type_Conversion =>
            return Asis.A_Type_Conversion;
         when F.A_Qualified_Expression =>
            return Asis.A_Qualified_Expression;
         when F.An_Allocation_From_Subtype =>
            return Asis.An_Allocation_From_Subtype;
         when F.An_Allocation_From_Qualified_Expression =>
            return Asis.An_Allocation_From_Qualified_Expression;
         when others =>
            return Not_An_Expression;
      end case;
   end Expression_Kind;

   ----------------------
   -- Formal_Type_Kind --
   ----------------------

   function Formal_Type_Kind
     (Definition : in Asis.Formal_Type_Definition)
      return Asis.Formal_Type_Kinds
   is
   begin
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Formal_Type_Definition;
      else
         return Not_A_Formal_Type_Definition;
      end if;
   end Formal_Type_Kind;

   -----------------
   -- Has_Limited --
   -----------------

   function Has_Limited (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Limited;

   -----------------
   -- Has_Private --
   -----------------

   function Has_Private (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Private;

   ------------------
   -- Has_Abstract --
   ------------------

   function Has_Abstract (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
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
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Synchronized;

   -------------------
   -- Has_Protected --
   -------------------

   function Has_Protected (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Protected;

   ----------------
   -- Has_Tagged --
   ----------------

   function Has_Tagged (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Tagged;

   --------------
   -- Has_Task --
   --------------

   function Has_Task (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Task;

   ------------------------
   -- Has_Null_Exclusion --
   ------------------------

   function Has_Null_Exclusion (Element : Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Null_Exclusion;

   ----------
   -- Hash --
   ----------

   function Hash (Element : in Asis.Element) return Asis.ASIS_Integer is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return 0;
      else
         return 0;
      end if;
   end Hash;

   --------------------
   -- Interface_Kind --
   --------------------

   function Interface_Kind
     (Definition : Asis.Definition)
     return Asis.Interface_Kinds is
   begin
      if Assigned (Definition) then
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
      return Boolean
   is
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
   begin
      Raise_Not_Implemented ("");
      return False;
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : in Asis.Element;
      Right : in Asis.Element)
      return Boolean
   is
      use type Gela.Elements.Element_Access;
   begin
      return Left.Data = Right.Data;
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
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Is_Null_Procedure;

   -------------------------
   -- Is_Part_Of_Implicit --
   -------------------------

   function Is_Part_Of_Implicit (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Is_Part_Of_Implicit;

   --------------------------
   -- Is_Part_Of_Inherited --
   --------------------------

   function Is_Part_Of_Inherited
     (Element : in Asis.Element)
      return Boolean
   is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Is_Part_Of_Inherited;

   -------------------------
   -- Is_Part_Of_Instance --
   -------------------------

   function Is_Part_Of_Instance (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         return False;
      else
         return False;
      end if;
   end Is_Part_Of_Instance;

   ---------------
   -- Mode_Kind --
   ---------------

   function Mode_Kind
     (Declaration : in Asis.Declaration)
      return Asis.Mode_Kinds
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_A_Mode;
      else
         return Not_A_Mode;
      end if;
   end Mode_Kind;

   -------------------
   -- Operator_Kind --
   -------------------

   function Operator_Kind
     (Element : in Asis.Element)
      return Asis.Operator_Kinds
   is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return Not_An_Operator;
      else
         return Not_An_Operator;
      end if;
   end Operator_Kind;

   ---------------
   -- Path_Kind --
   ---------------

   function Path_Kind (Path : in Asis.Path) return Asis.Path_Kinds is
   begin
      if Assigned (Path) then
         Raise_Not_Implemented ("");
         return Not_A_Path;
      else
         return Not_A_Path;
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
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Pragma_Argument_Associations;

   -----------------
   -- Pragma_Kind --
   -----------------

   function Pragma_Kind
     (Pragma_Element : in Asis.Pragma_Element)
      return Asis.Pragma_Kinds
   is
   begin
      if Assigned (Pragma_Element) then
         return Not_A_Pragma;
      else
         return Not_A_Pragma;
      end if;
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
      Raise_Not_Implemented ("");
      return "";
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
      Raise_Not_Implemented ("");
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
      if Assigned (Clause) then
         Raise_Not_Implemented ("");
         return Not_A_Representation_Clause;
      else
         return Not_A_Representation_Clause;
      end if;
   end Representation_Clause_Kind;

   --------------------
   -- Root_Type_Kind --
   --------------------

   function Root_Type_Kind
     (Definition : in Asis.Root_Type_Definition)
      return Asis.Root_Type_Kinds
   is
   begin
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Root_Type_Definition;
      else
         return Not_A_Root_Type_Definition;
      end if;
   end Root_Type_Kind;

   --------------------
   -- Statement_Kind --
   --------------------

   function Statement_Kind
     (Statement : in Asis.Statement)
      return Asis.Statement_Kinds
   is
   begin
      if Assigned (Statement) then
         Raise_Not_Implemented ("");
         return Not_A_Statement;
      else
         return Not_A_Statement;
      end if;
   end Statement_Kind;

   ----------------
   -- Trait_Kind --
   ----------------

   function Trait_Kind
     (Element : in Asis.Element)
      return Asis.Trait_Kinds
   is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return Not_A_Trait;
      else
         return Not_A_Trait;
      end if;
   end Trait_Kind;

   ---------------
   -- Type_Kind --
   ---------------

   function Type_Kind
     (Definition : in Asis.Type_Definition)
      return Asis.Type_Kinds
   is
   begin
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Type_Definition;
      else
         return Not_A_Type_Definition;
      end if;
   end Type_Kind;

   ----------------------
   -- Unit_Declaration --
   ----------------------

   function Unit_Declaration
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Declaration
   is
      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Unit : Gela.Elements.Element_Access;
         end record;

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access);

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access);

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access);

      end Get;

      package body Get is

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access)
         is
            Result : constant Gela.Elements.Library_Unit_Bodies.
              Library_Unit_Body_Access := Node.Unit_Declaration;
         begin
            Self.Unit := Gela.Elements.Element_Access (Result);
         end Compilation_Unit_Body;

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access)
         is
            Result : constant Gela.Elements.Library_Unit_Declarations.
              Library_Unit_Declaration_Access := Node.Unit_Declaration;
         begin
            Self.Unit := Gela.Elements.Element_Access (Result);
         end Compilation_Unit_Declaration;

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access)
         is
            Result : constant Gela.Elements.Proper_Bodies.Proper_Body_Access :=
              Node.Unit_Declaration;
         begin
            Self.Unit := Gela.Elements.Element_Access (Result);
         end Subunit;

      end Get;

      Tree : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
      V    : aliased Get.Visiter;
   begin
      Check_Nil_Unit (Compilation_Unit, "Unit_Declaration");
      Tree := Compilation_Unit.Data.Tree;
      Tree.Visit (V);

      return (Data => V.Unit);
   end Unit_Declaration;

end Asis.Elements;


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