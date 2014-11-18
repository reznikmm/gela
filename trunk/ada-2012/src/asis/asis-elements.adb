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

with System.Storage_Elements;

with Gela.Compilations;
with Gela.Compilation_Unit_Sets;
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
with Gela.Elements.Defining_Character_Literals;
with Gela.Elements.Defining_Enumeration_Literals;
with Gela.Elements.Defining_Expanded_Unit_Names;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Operator_Symbols;
with Gela.Lexical_Types;
with Gela.Elements.Abort_Statements;
with Gela.Elements.Accept_Statements;
with Gela.Elements.Assignment_Statements;
with Gela.Elements.Asynchronous_Selects;
with Gela.Elements.At_Clauses;
with Gela.Elements.Attribute_Definition_Clauses;
with Gela.Elements.Choice_Parameter_Specifications;
with Gela.Elements.Block_Statements;
with Gela.Elements.Case_Statements;
with Gela.Elements.Component_Clauses;
with Gela.Elements.Component_Declarations;
with Gela.Elements.Delay_Statements;
with Gela.Elements.Discriminant_Specifications;
with Gela.Elements.Element_Iterator_Specifications;
with Gela.Elements.Entry_Bodies;
with Gela.Elements.Entry_Declarations;
with Gela.Elements.Entry_Index_Specifications;
with Gela.Elements.Exception_Declarations;
with Gela.Elements.Exception_Handlers;
with Gela.Elements.Exception_Renaming_Declarations;
with Gela.Elements.Exit_Statements;
with Gela.Elements.Extended_Return_Statements;
with Gela.Elements.For_Loop_Statements;
with Gela.Elements.Formal_Function_Declarations;
with Gela.Elements.Formal_Incomplete_Type_Declarations;
with Gela.Elements.Formal_Object_Declarations;
with Gela.Elements.Formal_Package_Declarations;
with Gela.Elements.Formal_Procedure_Declarations;
with Gela.Elements.Formal_Type_Declarations;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Function_Bodies;
with Gela.Elements.Function_Declarations;
with Gela.Elements.Function_Instantiations;
with Gela.Elements.Generalized_Iterator_Specifications;
with Gela.Elements.Generic_Function_Declarations;
with Gela.Elements.Generic_Function_Renamings;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Generic_Package_Renamings;
with Gela.Elements.Generic_Procedure_Declarations;
with Gela.Elements.Generic_Procedure_Renamings;
with Gela.Elements.Goto_Statements;
with Gela.Elements.If_Statements;
with Gela.Elements.Incomplete_Type_Declarations;
with Gela.Elements.Loop_Parameter_Specifications;
with Gela.Elements.Loop_Statements;
with Gela.Elements.Null_Components;
with Gela.Elements.Null_Statements;
with Gela.Elements.Number_Declarations;
with Gela.Elements.Object_Declarations;
with Gela.Elements.Object_Renaming_Declarations;
with Gela.Elements.Package_Bodies;
with Gela.Elements.Package_Body_Stubs;
with Gela.Elements.Package_Declarations;
with Gela.Elements.Package_Instantiations;
with Gela.Elements.Package_Renaming_Declarations;
with Gela.Elements.Parameter_Specifications;
with Gela.Elements.Pragma_Nodes;
with Gela.Elements.Private_Extension_Declarations;
with Gela.Elements.Private_Type_Declarations;
with Gela.Elements.Procedure_Bodies;
with Gela.Elements.Procedure_Call_Statements;
with Gela.Elements.Procedure_Declarations;
with Gela.Elements.Procedure_Instantiations;
with Gela.Elements.Protected_Bodies;
with Gela.Elements.Return_Object_Specifications;
with Gela.Elements.Protected_Body_Stubs;
with Gela.Elements.Protected_Type_Declarations;
with Gela.Elements.Raise_Statements;
with Gela.Elements.Record_Representation_Clauses;
with Gela.Elements.Requeue_Statements;
with Gela.Elements.Selective_Accepts;
with Gela.Elements.Simple_Return_Statements;
with Gela.Elements.Single_Protected_Declarations;
with Gela.Elements.Single_Task_Declarations;
with Gela.Elements.Task_Bodies;
with Gela.Elements.Subtype_Declarations;
with Gela.Elements.Task_Body_Stubs;
with Gela.Elements.Task_Type_Declarations;
with Gela.Elements.Terminate_Alternative_Statements;
with Gela.Elements.Use_Package_Clauses;
with Gela.Elements.Use_Type_Clauses;
with Gela.Elements.Variant_Parts;
with Gela.Elements.While_Loop_Statements;
with Gela.Elements.With_Clauses;

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
      use type Gela.Compilation_Units.Compilation_Unit_Access;

      procedure Find
        (List : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
         Unit : out Gela.Compilation_Units.Compilation_Unit_Access);

      Comp    : Gela.Compilations.Compilation_Access;
      From    : Gela.Lexical_Types.Token_Count;
      To      : Gela.Lexical_Types.Token_Count;

      ----------
      -- Find --
      ----------

      procedure Find
        (List : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
         Unit : out Gela.Compilation_Units.Compilation_Unit_Access)
      is
         use Gela.Compilations;
         use type Gela.Lexical_Types.Token_Count;
         Tree    : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
         Cursor  : Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class :=
           List.First;
      begin
         while Cursor.Has_Element loop
            Unit := Cursor.Element;
            Tree := Unit.Tree;

            if Unit.Compilation = Comp
              and then Tree.First_Token <= From
              and then Tree.Last_Token >= To
            then
               return;
            end if;

            Cursor.Next;
         end loop;

         Unit := null;
      end Find;

      Context : Gela.Contexts.Context_Access;
      Unit    : Gela.Compilation_Units.Compilation_Unit_Access;

   begin
      Check_Nil_Element (Element, "Enclosing_Compilation_Unit");
      Comp := Element.Data.Enclosing_Compilation;
      Context := Comp.Context;
      From := Element.Data.First_Token;
      To := Element.Data.Last_Token;
      Find (Context.Library_Unit_Declarations, Unit);

      if Unit = null then
         Find (Context.Compilation_Unit_Bodies, Unit);
      end if;

      --  Raise_Not_Implemented ("");
      return (Data => Unit);
   end Enclosing_Compilation_Unit;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   function Enclosing_Element
     (Element : in Asis.Element)
      return Asis.Element
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Abort_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Abort_Statements.
              Abort_Statement_Access);

         overriding procedure Accept_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Accept_Statements.
              Accept_Statement_Access);

         overriding procedure Assignment_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Assignment_Statements.
              Assignment_Statement_Access);

         overriding procedure Asynchronous_Select
           (Self : in out Visiter;
            Node : not null Gela.Elements.Asynchronous_Selects.
              Asynchronous_Select_Access);

         overriding procedure At_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.At_Clauses.At_Clause_Access);

         overriding procedure Attribute_Definition_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Attribute_Definition_Clauses.
              Attribute_Definition_Clause_Access);

         overriding procedure Block_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Block_Statements.
              Block_Statement_Access);

         overriding procedure Case_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Case_Statements.
              Case_Statement_Access);

         overriding procedure Choice_Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Choice_Parameter_Specifications.
              Choice_Parameter_Specification_Access);

         overriding procedure Component_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Clauses.
              Component_Clause_Access);

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access);

         overriding procedure Defining_Character_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Character_Literals.
              Defining_Character_Literal_Access);

         overriding procedure Defining_Enumeration_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Enumeration_Literals.
              Defining_Enumeration_Literal_Access);

         overriding procedure Defining_Expanded_Unit_Name
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
              Defining_Expanded_Unit_Name_Access);

         overriding procedure Defining_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access);

         overriding procedure Defining_Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Operator_Symbols.
              Defining_Operator_Symbol_Access);

         overriding procedure Delay_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Delay_Statements.
              Delay_Statement_Access);

         overriding procedure Discriminant_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Access);

         overriding procedure Element_Iterator_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Element_Iterator_Specifications.
              Element_Iterator_Specification_Access);

         overriding procedure Entry_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access);

         overriding procedure Entry_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Declarations.
              Entry_Declaration_Access);

         overriding procedure Entry_Index_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Index_Specifications.
              Entry_Index_Specification_Access);

         overriding procedure Exception_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exception_Declarations.
              Exception_Declaration_Access);

         overriding procedure Exception_Handler
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exception_Handlers.
              Exception_Handler_Access);

         overriding procedure Exception_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exception_Renaming_Declarations.
              Exception_Renaming_Declaration_Access);

         overriding procedure Exit_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exit_Statements.
              Exit_Statement_Access);

         overriding procedure Extended_Return_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Extended_Return_Statements.
              Extended_Return_Statement_Access);

         overriding procedure For_Loop_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.For_Loop_Statements.
              For_Loop_Statement_Access);

         overriding procedure Formal_Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Function_Declarations.
              Formal_Function_Declaration_Access);

         overriding procedure Formal_Incomplete_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Incomplete_Type_Declarations.
              Formal_Incomplete_Type_Declaration_Access);

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access);

         overriding procedure Formal_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Package_Declarations.
              Formal_Package_Declaration_Access);

         overriding procedure Formal_Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Procedure_Declarations.
              Formal_Procedure_Declaration_Access);

         overriding procedure Formal_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Type_Declarations.
              Formal_Type_Declaration_Access);

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access);

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access);

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access);

         overriding procedure Function_Instantiation
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Instantiations.
              Function_Instantiation_Access);

         overriding procedure Generalized_Iterator_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generalized_Iterator_Specifications.
              Generalized_Iterator_Specification_Access);

         overriding procedure Generic_Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Function_Declarations.
              Generic_Function_Declaration_Access);

         overriding procedure Generic_Function_Renaming
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Function_Renamings.
              Generic_Function_Renaming_Access);

         overriding procedure Generic_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Declarations.
              Generic_Package_Declaration_Access);

         overriding procedure Generic_Package_Renaming
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Renamings.
              Generic_Package_Renaming_Access);

         overriding procedure Generic_Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Procedure_Declarations.
              Generic_Procedure_Declaration_Access);

         overriding procedure Generic_Procedure_Renaming
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Procedure_Renamings.
              Generic_Procedure_Renaming_Access);

         overriding procedure Goto_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Goto_Statements.
              Goto_Statement_Access);

         overriding procedure If_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.If_Statements.If_Statement_Access);

         overriding procedure Incomplete_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Incomplete_Type_Declarations.
              Incomplete_Type_Declaration_Access);

         overriding procedure Loop_Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Loop_Parameter_Specifications.
              Loop_Parameter_Specification_Access);

         overriding procedure Loop_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Loop_Statements.
              Loop_Statement_Access);

         overriding procedure Null_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Null_Components.
              Null_Component_Access);

         overriding procedure Null_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Null_Statements.
              Null_Statement_Access);

         overriding procedure Number_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Number_Declarations.
              Number_Declaration_Access);

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access);

         overriding procedure Object_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Renaming_Declarations.
              Object_Renaming_Declaration_Access);

         overriding procedure Package_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Bodies.Package_Body_Access);

         overriding procedure Package_Body_Stub
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Body_Stubs.
              Package_Body_Stub_Access);

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access);

         overriding procedure Package_Instantiation
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Instantiations.
              Package_Instantiation_Access);

         overriding procedure Package_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Renaming_Declarations.
              Package_Renaming_Declaration_Access);

         overriding procedure Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Access);

         overriding procedure Pragma_Node
           (Self : in out Visiter;
            Node : not null Gela.Elements.Pragma_Nodes.Pragma_Node_Access);

         overriding procedure Private_Extension_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Private_Extension_Declarations.
              Private_Extension_Declaration_Access);

         overriding procedure Private_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Private_Type_Declarations.
              Private_Type_Declaration_Access);

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access);

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access);

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access);

         overriding procedure Procedure_Instantiation
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Instantiations.
              Procedure_Instantiation_Access);

         overriding procedure Protected_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Protected_Bodies.
              Protected_Body_Access);

         overriding procedure Protected_Body_Stub
           (Self : in out Visiter;
            Node : not null Gela.Elements.Protected_Body_Stubs.
              Protected_Body_Stub_Access);

         overriding procedure Protected_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Protected_Type_Declarations.
              Protected_Type_Declaration_Access);

         overriding procedure Raise_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Raise_Statements.
              Raise_Statement_Access);

         overriding procedure Record_Representation_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Representation_Clauses.
              Record_Representation_Clause_Access);

         overriding procedure Requeue_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Requeue_Statements.
              Requeue_Statement_Access);

         overriding procedure Return_Object_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Return_Object_Specifications.
              Return_Object_Specification_Access);

         overriding procedure Selective_Accept
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selective_Accepts.
              Selective_Accept_Access);

         overriding procedure Simple_Return_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Simple_Return_Statements.
              Simple_Return_Statement_Access);

         overriding procedure Single_Protected_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Protected_Declarations.
              Single_Protected_Declaration_Access);

         overriding procedure Single_Task_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Task_Declarations.
              Single_Task_Declaration_Access);

         overriding procedure Subtype_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subtype_Declarations.
              Subtype_Declaration_Access);

         overriding procedure Task_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Bodies.Task_Body_Access);

         overriding procedure Task_Body_Stub
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Body_Stubs.
              Task_Body_Stub_Access);

         overriding procedure Task_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Type_Declarations.
              Task_Type_Declaration_Access);

         overriding procedure Terminate_Alternative_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Terminate_Alternative_Statements.
              Terminate_Alternative_Statement_Access);

         overriding procedure Use_Package_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Package_Clauses.
              Use_Package_Clause_Access);

         overriding procedure Use_Type_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Type_Clauses.
              Use_Type_Clause_Access);

         overriding procedure Variant_Part
           (Self : in out Visiter;
            Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access);

         overriding procedure While_Loop_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.While_Loop_Statements.
              While_Loop_Statement_Access);

         overriding procedure With_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access);

      end Get;

      package body Get is

         overriding procedure Abort_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Abort_Statements.
              Abort_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Abort_Statement;

         overriding procedure Accept_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Accept_Statements.
              Accept_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Accept_Statement;

         overriding procedure Assignment_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Assignment_Statements.
              Assignment_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Assignment_Statement;

         overriding procedure Asynchronous_Select
           (Self : in out Visiter;
            Node : not null Gela.Elements.Asynchronous_Selects.
              Asynchronous_Select_Access) is
         begin
            Self.Result := Node.Parent;
         end Asynchronous_Select;

         overriding procedure At_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.At_Clauses.At_Clause_Access) is
         begin
            Self.Result := Node.Parent;
         end At_Clause;

         overriding procedure Attribute_Definition_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Attribute_Definition_Clauses.
              Attribute_Definition_Clause_Access) is
         begin
            Self.Result := Node.Parent;
         end Attribute_Definition_Clause;

         overriding procedure Block_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Block_Statements.
              Block_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Block_Statement;

         overriding procedure Case_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Case_Statements.
              Case_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Case_Statement;

         overriding procedure Choice_Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Choice_Parameter_Specifications.
              Choice_Parameter_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Choice_Parameter_Specification;

         overriding procedure Component_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Clauses.
              Component_Clause_Access) is
         begin
            Self.Result := Node.Parent;
         end Component_Clause;

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Component_Declaration;

         overriding procedure Defining_Character_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Character_Literals.
              Defining_Character_Literal_Access) is
         begin
            Self.Result := Node.Parent;
         end Defining_Character_Literal;

         overriding procedure Defining_Enumeration_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Enumeration_Literals.
              Defining_Enumeration_Literal_Access) is
         begin
            Self.Result := Node.Parent;
         end Defining_Enumeration_Literal;

         overriding procedure Defining_Expanded_Unit_Name
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
              Defining_Expanded_Unit_Name_Access) is
         begin
            Self.Result := Node.Parent;
         end Defining_Expanded_Unit_Name;

         overriding procedure Defining_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access) is
         begin
            Self.Result := Node.Parent;
         end Defining_Identifier;

         overriding procedure Defining_Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Operator_Symbols.
              Defining_Operator_Symbol_Access) is
         begin
            Self.Result := Node.Parent;
         end Defining_Operator_Symbol;

         overriding procedure Delay_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Delay_Statements.
              Delay_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Delay_Statement;

         overriding procedure Discriminant_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Discriminant_Specification;

         overriding procedure Element_Iterator_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Element_Iterator_Specifications.
              Element_Iterator_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Element_Iterator_Specification;

         overriding procedure Entry_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access) is
         begin
            Self.Result := Node.Parent;
         end Entry_Body;

         overriding procedure Entry_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Declarations.
              Entry_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Entry_Declaration;

         overriding procedure Entry_Index_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Index_Specifications.
              Entry_Index_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Entry_Index_Specification;

         overriding procedure Exception_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exception_Declarations.
              Exception_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Exception_Declaration;

         overriding procedure Exception_Handler
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exception_Handlers.
              Exception_Handler_Access) is
         begin
            Self.Result := Node.Parent;
         end Exception_Handler;

         overriding procedure Exception_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exception_Renaming_Declarations.
              Exception_Renaming_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Exception_Renaming_Declaration;

         overriding procedure Exit_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Exit_Statements.
              Exit_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Exit_Statement;

         overriding procedure Extended_Return_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Extended_Return_Statements.
              Extended_Return_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Extended_Return_Statement;

         overriding procedure For_Loop_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.For_Loop_Statements.
              For_Loop_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end For_Loop_Statement;

         overriding procedure Formal_Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Function_Declarations.
              Formal_Function_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Formal_Function_Declaration;

         overriding procedure Formal_Incomplete_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Incomplete_Type_Declarations.
              Formal_Incomplete_Type_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Formal_Incomplete_Type_Declaration;

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Formal_Object_Declaration;

         overriding procedure Formal_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Package_Declarations.
              Formal_Package_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Formal_Package_Declaration;

         overriding procedure Formal_Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Procedure_Declarations.
              Formal_Procedure_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Formal_Procedure_Declaration;

         overriding procedure Formal_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Type_Declarations.
              Formal_Type_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Formal_Type_Declaration;

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Full_Type_Declaration;

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access) is
         begin
            Self.Result := Node.Parent;
         end Function_Body;

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Function_Declaration;

         overriding procedure Function_Instantiation
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Instantiations.
              Function_Instantiation_Access) is
         begin
            Self.Result := Node.Parent;
         end Function_Instantiation;

         overriding procedure Generalized_Iterator_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generalized_Iterator_Specifications.
              Generalized_Iterator_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Generalized_Iterator_Specification;

         overriding procedure Generic_Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Function_Declarations.
              Generic_Function_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Generic_Function_Declaration;

         overriding procedure Generic_Function_Renaming
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Function_Renamings.
              Generic_Function_Renaming_Access) is
         begin
            Self.Result := Node.Parent;
         end Generic_Function_Renaming;

         overriding procedure Generic_Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Declarations.
              Generic_Package_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Generic_Package_Declaration;

         overriding procedure Generic_Package_Renaming
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Package_Renamings.
              Generic_Package_Renaming_Access) is
         begin
            Self.Result := Node.Parent;
         end Generic_Package_Renaming;

         overriding procedure Generic_Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Procedure_Declarations.
              Generic_Procedure_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Generic_Procedure_Declaration;

         overriding procedure Generic_Procedure_Renaming
           (Self : in out Visiter;
            Node : not null Gela.Elements.Generic_Procedure_Renamings.
              Generic_Procedure_Renaming_Access) is
         begin
            Self.Result := Node.Parent;
         end Generic_Procedure_Renaming;

         overriding procedure Goto_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Goto_Statements.
              Goto_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Goto_Statement;

         overriding procedure If_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.If_Statements.If_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end If_Statement;

         overriding procedure Incomplete_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Incomplete_Type_Declarations.
              Incomplete_Type_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Incomplete_Type_Declaration;

         overriding procedure Loop_Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Loop_Parameter_Specifications.
              Loop_Parameter_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Loop_Parameter_Specification;

         overriding procedure Loop_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Loop_Statements.
              Loop_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Loop_Statement;

         overriding procedure Null_Component
           (Self : in out Visiter;
            Node : not null Gela.Elements.Null_Components.
              Null_Component_Access) is
         begin
            Self.Result := Node.Parent;
         end Null_Component;

         overriding procedure Null_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Null_Statements.
              Null_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Null_Statement;

         overriding procedure Number_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Number_Declarations.
              Number_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Number_Declaration;

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Object_Declaration;

         overriding procedure Object_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Renaming_Declarations.
              Object_Renaming_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Object_Renaming_Declaration;

         overriding procedure Package_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Bodies.Package_Body_Access)
         is
         begin
            Self.Result := Node.Parent;
         end Package_Body;

         overriding procedure Package_Body_Stub
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Body_Stubs.
              Package_Body_Stub_Access) is
         begin
            Self.Result := Node.Parent;
         end Package_Body_Stub;

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Package_Declaration;

         overriding procedure Package_Instantiation
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Instantiations.
              Package_Instantiation_Access) is
         begin
            Self.Result := Node.Parent;
         end Package_Instantiation;

         overriding procedure Package_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Renaming_Declarations.
              Package_Renaming_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Package_Renaming_Declaration;

         overriding procedure Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Parameter_Specification;

         overriding procedure Pragma_Node
           (Self : in out Visiter;
            Node : not null Gela.Elements.Pragma_Nodes.Pragma_Node_Access) is
         begin
            Self.Result := Node.Parent;
         end Pragma_Node;

         overriding procedure Private_Extension_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Private_Extension_Declarations.
              Private_Extension_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Private_Extension_Declaration;

         overriding procedure Private_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Private_Type_Declarations.
              Private_Type_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Private_Type_Declaration;

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access) is
         begin
            Self.Result := Node.Parent;
         end Procedure_Body;

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Procedure_Call_Statement;

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Procedure_Declaration;

         overriding procedure Procedure_Instantiation
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Instantiations.
              Procedure_Instantiation_Access) is
         begin
            Self.Result := Node.Parent;
         end Procedure_Instantiation;

         overriding procedure Protected_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Protected_Bodies.
              Protected_Body_Access) is
         begin
            Self.Result := Node.Parent;
         end Protected_Body;

         overriding procedure Protected_Body_Stub
           (Self : in out Visiter;
            Node : not null Gela.Elements.Protected_Body_Stubs.
              Protected_Body_Stub_Access) is
         begin
            Self.Result := Node.Parent;
         end Protected_Body_Stub;

         overriding procedure Protected_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Protected_Type_Declarations.
              Protected_Type_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Protected_Type_Declaration;

         overriding procedure Raise_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Raise_Statements.
              Raise_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Raise_Statement;

         overriding procedure Record_Representation_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Representation_Clauses.
              Record_Representation_Clause_Access) is
         begin
            Self.Result := Node.Parent;
         end Record_Representation_Clause;

         overriding procedure Requeue_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Requeue_Statements.
              Requeue_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Requeue_Statement;

         overriding procedure Return_Object_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Return_Object_Specifications.
              Return_Object_Specification_Access) is
         begin
            Self.Result := Node.Parent;
         end Return_Object_Specification;

         overriding procedure Selective_Accept
           (Self : in out Visiter;
            Node : not null Gela.Elements.Selective_Accepts.
              Selective_Accept_Access) is
         begin
            Self.Result := Node.Parent;
         end Selective_Accept;

         overriding procedure Simple_Return_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Simple_Return_Statements.
              Simple_Return_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Simple_Return_Statement;

         overriding procedure Single_Protected_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Protected_Declarations.
              Single_Protected_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Single_Protected_Declaration;

         overriding procedure Single_Task_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Task_Declarations.
              Single_Task_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Single_Task_Declaration;

         overriding procedure Subtype_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subtype_Declarations.
              Subtype_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Subtype_Declaration;

         overriding procedure Task_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Bodies.Task_Body_Access) is
         begin
            Self.Result := Node.Parent;
         end Task_Body;

         overriding procedure Task_Body_Stub
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Body_Stubs.
              Task_Body_Stub_Access) is
         begin
            Self.Result := Node.Parent;
         end Task_Body_Stub;

         overriding procedure Task_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Type_Declarations.
              Task_Type_Declaration_Access) is
         begin
            Self.Result := Node.Parent;
         end Task_Type_Declaration;

         overriding procedure Terminate_Alternative_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Terminate_Alternative_Statements.
              Terminate_Alternative_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end Terminate_Alternative_Statement;

         overriding procedure Use_Package_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Package_Clauses.
              Use_Package_Clause_Access) is
         begin
            Self.Result := Node.Parent;
         end Use_Package_Clause;

         overriding procedure Use_Type_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.Use_Type_Clauses.
              Use_Type_Clause_Access) is
         begin
            Self.Result := Node.Parent;
         end Use_Type_Clause;

         overriding procedure Variant_Part
           (Self : in out Visiter;
            Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access) is
         begin
            Self.Result := Node.Parent;
         end Variant_Part;

         overriding procedure While_Loop_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.While_Loop_Statements.
              While_Loop_Statement_Access) is
         begin
            Self.Result := Node.Parent;
         end While_Loop_Statement;

         overriding procedure With_Clause
           (Self : in out Visiter;
            Node : not null Gela.Elements.With_Clauses.With_Clause_Access) is
         begin
            Self.Result := Node.Parent;
         end With_Clause;

      end Get;

      V : Get.Visiter;
      Next : Asis.Element := Element;
   begin
      Check_Nil_Element (Element, "Enclosing_Element");
      loop
         V.Result := null;
         Next.Data.Visit (V);
         Next := (Data => V.Result);

         if not Assigned (Next) or else not Auxilary (Next) then
            return Next;
         end if;
      end loop;
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
      subtype Integer_Address is System.Storage_Elements.Integer_Address;
      use type Integer_Address;
      X : Integer_Address;
   begin
      if Assigned (Element) then
         X := System.Storage_Elements.To_Integer (Element.Data.all'Address);
         X := X and Integer_Address (ASIS_Integer'Last);
         return ASIS_Integer (X);
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
