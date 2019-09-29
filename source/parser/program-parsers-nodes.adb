--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vector_Factories;
with Program.Element_Vectors;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Names;
with Program.Elements.Definitions;
with Program.Elements.Enumeration_Literal_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Operator_Symbols;
with Program.Storage_Pools;
with Program.Units.Declarations;

package body Program.Parsers.Nodes is

   generic
      type Vector_Access is private;

      with function Create_Vector
        (Self : Program.Element_Vector_Factories.Element_Vector_Factory;
         Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
       return Vector_Access;
   function Generic_Vector_Cast
     (Value   : access constant Element_Vectors.Vector;
      Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle)
         return Vector_Access;

   type Forward_Iterator;

   function Iter
     (V : access constant Element_Vectors.Vector) return Forward_Iterator;

   type Forward_Iterator is
     new Program.Element_Vectors.Iterators.Forward_Iterator with
   record
      Vector : access constant Element_Vectors.Vector;
   end record;

   overriding function First
     (Self : Forward_Iterator)
      return Program.Element_Vectors.Element_Cursor;

   overriding function Next
     (Self     : Forward_Iterator;
      Position : Program.Element_Vectors.Element_Cursor)
      return Program.Element_Vectors.Element_Cursor;

   function New_Element_Sequence (Self : Node_Factory'Class) return Node;

   procedure Prepend
     (Self : Node_Factory'Class;
      List : in out Node;
      Item : Node);

   procedure Append
     (Self : Node_Factory'Class;
      List : in out Node;
      Item : Node);

   -----------
   -- First --
   -----------

   overriding function First
     (Self : Forward_Iterator)
      return Program.Element_Vectors.Element_Cursor is
   begin
      return
        (Element   => (if Self.Vector.Is_Empty then null
                       else Self.Vector.Element (1)),
         Delimiter => null,
         Index     => 1,
         Is_Last   => Self.Vector.Last_Index = 1);
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self     : Forward_Iterator;
      Position : Program.Element_Vectors.Element_Cursor)
      return Program.Element_Vectors.Element_Cursor
   is
      Index : constant Positive := Position.Index + 1;
   begin
      if Self.Vector.Last_Index < Index then
         return (null, null, Index, False);
      else
         return
           (Element   => Self.Vector.Element (Index),
            Delimiter => null,
            Index     => Index,
            Is_Last   => Self.Vector.Last_Index = Index);
      end if;
   end Next;

   ----------
   -- Iter --
   ----------

   function Iter
     (V : access constant Element_Vectors.Vector) return Forward_Iterator is
   begin
      return (Vector => V);
   end Iter;

   -------------------------
   -- Generic_Vector_Cast --
   -------------------------

   function Generic_Vector_Cast
     (Value   : access constant Element_Vectors.Vector;
      Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle)
         return Vector_Access
   is
      Fact : Program.Element_Vector_Factories.Element_Vector_Factory (Subpool);
   begin
      return Create_Vector (Fact, Iter (Value));
   end Generic_Vector_Cast;

   function To_Aspect_Specification_Vector is new Generic_Vector_Cast
     (Vector_Access => Program.Elements
         .Aspect_Specifications.Aspect_Specification_Vector_Access,
      Create_Vector =>
         Program.Element_Vector_Factories.Create_Aspect_Specification_Vector);

   function To_Enumeration_Literal_Specification_Vector is
     new Generic_Vector_Cast
       (Vector_Access => Program.Elements
           .Enumeration_Literal_Specifications
              .Enumeration_Literal_Specification_Vector_Access,
        Create_Vector => Program.Element_Vector_Factories
           .Create_Enumeration_Literal_Specification_Vector);

   function To_Element_Vector is new Generic_Vector_Cast
     (Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Create_Vector => Program.Element_Vector_Factories.Create_Element_Vector);

   pragma Warnings (Off);

   ---------------------
   -- Abort_Statement --
   ---------------------

   function Abort_Statement
     (Self : Node_Factory'Class; Abort_Token : Node; Aborted_Tasks : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Abort_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Abort_Statement";
   end Abort_Statement;

   ----------------------
   -- Accept_Statement --
   ----------------------

   function Accept_Statement
     (Self                     : Node_Factory'Class; Accept_Token : Node;
      Accept_Entry_Direct_Name : Node; Left_Parenthesis_Token : Node;
      Accept_Entry_Index       : Node; Right_Parenthesis_Token : Node;
      Lp_Token : Node; Accept_Parameters : Node; Rp_Token : Node;
      Do_Token : Node; Accept_Body_Statements : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token          : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Accept_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function Accept_Statement";
   end Accept_Statement;

   -----------------------------------
   -- Access_To_Function_Definition --
   -----------------------------------

   function Access_To_Function_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token      : Node; Protected_Token : Node; Function_Token : Node;
      Lp_Token          : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token          : Node; Return_Token : Node; Return_Not_Token : Node;
      Return_Null_Token : Node; Access_To_Function_Result_Subtype : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Access_To_Function_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Access_To_Function_Definition";
   end Access_To_Function_Definition;

   ---------------------------------
   -- Access_To_Object_Definition --
   ---------------------------------

   function Access_To_Object_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Constant_Token : Node; Subtype_Indication : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Access_To_Object_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Access_To_Object_Definition";
   end Access_To_Object_Definition;

   ------------------------------------
   -- Access_To_Procedure_Definition --
   ------------------------------------

   function Access_To_Procedure_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Protected_Token : Node; Procedure_Token : Node;
      Lp_Token     : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token     : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Access_To_Procedure_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Access_To_Procedure_Definition";
   end Access_To_Procedure_Definition;

   ---------------
   -- Allocator --
   ---------------

   function Allocator
     (Self                    : Node_Factory'Class; New_Token : Node;
      Left_Parenthesis_Token  : Node; Subpool_Name : Node;
      Right_Parenthesis_Token : Node; Subtype_Or_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Allocator unimplemented");
      return raise Program_Error with "Unimplemented function Allocator";
   end Allocator;

   ---------------------------------------------
   -- Anonymous_Access_To_Function_Definition --
   ---------------------------------------------

   function Anonymous_Access_To_Function_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token      : Node; Protected_Token : Node; Function_Token : Node;
      Lp_Token          : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token          : Node; Return_Token : Node; Return_Not_Token : Node;
      Return_Null_Token : Node; Access_To_Function_Result_Subtype : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Anonymous_Access_To_Function_Definition unimplemented");
      return raise Program_Error
        with "Unimplemented function Anonymous_Access_To_Function_Definition";
   end Anonymous_Access_To_Function_Definition;

   -------------------------------------------
   -- Anonymous_Access_To_Object_Definition --
   -------------------------------------------

   function Anonymous_Access_To_Object_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token                            : Node; Constant_Token : Node;
      Anonymous_Access_To_Object_Subtype_Mark : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Anonymous_Access_To_Object_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Anonymous_Access_To_Object_Definition";
   end Anonymous_Access_To_Object_Definition;

   ----------------------------------------------
   -- Anonymous_Access_To_Procedure_Definition --
   ----------------------------------------------

   function Anonymous_Access_To_Procedure_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Protected_Token : Node; Procedure_Token : Node;
      Lp_Token     : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token     : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Anonymous_Access_To_Procedure_Definition unimplemented");
      return raise Program_Error
        with "Unimplemented function Anonymous_Access_To_Procedure_Definition";
   end Anonymous_Access_To_Procedure_Definition;

   procedure Append
     (Self : Node_Factory'Class;
      List : in out Node;
      Item : Node) is
   begin
      List.Vector.Append (Item.Element);
   end Append;

   ---------------------------------
   -- Append_Aspect_Specification --
   ---------------------------------

   procedure Append_Aspect_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ------------------------
   -- Append_Association --
   ------------------------

   procedure Append_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -----------------------------------
   -- Append_Basic_Declarative_Item --
   -----------------------------------

   procedure Append_Basic_Declarative_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ---------------------------------
   -- Append_Case_Expression_Path --
   ---------------------------------

   procedure Append_Case_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ----------------------
   -- Append_Case_Path --
   ----------------------

   procedure Append_Case_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -----------------------------
   -- Append_Clause_Or_Pragma --
   -----------------------------

   procedure Append_Clause_Or_Pragma
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -----------------------------
   -- Append_Compilation_Unit --
   -----------------------------

   procedure Append_Compilation_Unit
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ---------------------------
   -- Append_Component_Item --
   ---------------------------

   procedure Append_Component_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -------------------------
   -- Append_Context_Item --
   -------------------------

   procedure Append_Context_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -----------------------------
   -- Append_Declarative_Item --
   -----------------------------

   procedure Append_Declarative_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   --------------------------------
   -- Append_Defining_Identifier --
   --------------------------------

   procedure Append_Defining_Identifier
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ----------------------------
   -- Append_Discrete_Choice --
   ----------------------------

   procedure Append_Discrete_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ----------------------------------------
   -- Append_Discrete_Subtype_Definition --
   ----------------------------------------

   procedure Append_Discrete_Subtype_Definition
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ---------------------------------------
   -- Append_Discriminant_Specification --
   ---------------------------------------

   procedure Append_Discriminant_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ----------------------------------------------
   -- Append_Enumeration_Literal_Specification --
   ----------------------------------------------

   procedure Append_Enumeration_Literal_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -----------------------------
   -- Append_Exception_Choice --
   -----------------------------

   procedure Append_Exception_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ------------------------------
   -- Append_Exception_Handler --
   ------------------------------

   procedure Append_Exception_Handler
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   --------------------------------
   -- Append_Generic_Association --
   --------------------------------

   procedure Append_Generic_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ---------------------------
   -- Append_Generic_Formal --
   ---------------------------

   procedure Append_Generic_Formal
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ------------------------------------
   -- Append_If_Else_Expression_Path --
   ------------------------------------

   procedure Append_If_Else_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -------------------------------
   -- Append_If_Elsif_Else_Path --
   -------------------------------

   procedure Append_If_Elsif_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ------------------------------
   -- Append_Membership_Choice --
   ------------------------------

   procedure Append_Membership_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -----------------
   -- Append_Name --
   -----------------

   procedure Append_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ------------------------------------
   -- Append_Parameter_Specification --
   ------------------------------------

   procedure Append_Parameter_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ----------------------------------------
   -- Append_Pragma_Argument_Association --
   ----------------------------------------

   procedure Append_Pragma_Argument_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ------------------------------
   -- Append_Program_Unit_Name --
   ------------------------------

   procedure Append_Program_Unit_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ------------------------------------------
   -- Append_Protected_Element_Declaration --
   ------------------------------------------

   procedure Append_Protected_Element_Declaration
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   --------------------------------------------
   -- Append_Protected_Operation_Declaration --
   --------------------------------------------

   procedure Append_Protected_Operation_Declaration
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -------------------------------------
   -- Append_Protected_Operation_Item --
   -------------------------------------

   procedure Append_Protected_Operation_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   --------------------------------
   -- Append_Select_Or_Else_Path --
   --------------------------------

   procedure Append_Select_Or_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -----------------------------------
   -- Append_Select_Then_Abort_Path --
   -----------------------------------

   procedure Append_Select_Then_Abort_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   ----------------------
   -- Append_Statement --
   ----------------------

   procedure Append_Statement
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   -------------------------
   -- Append_Subtype_Mark --
   -------------------------

   procedure Append_Subtype_Mark
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   --------------------
   -- Append_Variant --
   --------------------

   procedure Append_Variant
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Append;

   --------------------------
   -- Aspect_Specification --
   --------------------------

   function Aspect_Specification
     (Self : Node_Factory'Class; Aspect_Mark : Node; Arrow_Token : Node;
      Aspect_Definition : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Aspect_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Aspect_Specification";
   end Aspect_Specification;

   -----------------------------------
   -- Aspect_Specification_Sequence --
   -----------------------------------

   function Aspect_Specification_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------------
   -- Assignment_Statement --
   --------------------------

   function Assignment_Statement
     (Self             : Node_Factory'Class; Assignment_Variable_Name : Node;
      Assignment_Token : Node; Assignment_Expression : Node;
      Semicolon_Token  : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Assignment_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function Assignment_Statement";
   end Assignment_Statement;

   -----------------
   -- Association --
   -----------------

   function Association
     (Self        : Node_Factory'Class; Array_Component_Choices : Node;
      Arrow_Token : Node; Component_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Association unimplemented");
      return raise Program_Error with "Unimplemented function Association";
   end Association;

   ----------------------
   -- Association_List --
   ----------------------

   function Association_List
     (Self                          : Node_Factory'Class; Left_Token : Node;
      Record_Component_Associations : Node; Right_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Association_List unimplemented");
      return raise Program_Error
          with "Unimplemented function Association_List";
   end Association_List;

   --------------------------
   -- Association_Sequence --
   --------------------------

   function Association_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   -------------------------
   -- Asynchronous_Select --
   -------------------------

   function Asynchronous_Select
     (Self                         : Node_Factory'Class; Select_Token : Node;
      Asynchronous_Statement_Paths : Node; End_Token : Node; End_Select : Node;
      Semicolon_Token              : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Asynchronous_Select unimplemented");
      return raise Program_Error
          with "Unimplemented function Asynchronous_Select";
   end Asynchronous_Select;

   ---------------
   -- At_Clause --
   ---------------

   function At_Clause
     (Self                             : Node_Factory'Class; For_Token : Node;
      Representation_Clause_Name : Node; Use_Token : Node; At_Token : Node;
      Representation_Clause_Expression : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "At_Clause unimplemented");
      return raise Program_Error with "Unimplemented function At_Clause";
   end At_Clause;

   ---------------------------------
   -- Attribute_Definition_Clause --
   ---------------------------------

   function Attribute_Definition_Clause
     (Self                             : Node_Factory'Class; For_Token : Node;
      Representation_Clause_Name       : Node; Use_Token : Node;
      Representation_Clause_Expression : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Attribute_Definition_Clause unimplemented");
      return raise Program_Error
          with "Unimplemented function Attribute_Definition_Clause";
   end Attribute_Definition_Clause;

   -------------------------
   -- Attribute_Reference --
   -------------------------

   function Attribute_Reference
     (Self : Node_Factory'Class; Prefix : Node; Apostrophe_Token : Node;
      Attribute_Designator_Identifier  : Node;
      Attribute_Designator_Expressions : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Attribute_Reference unimplemented");
      return raise Program_Error
          with "Unimplemented function Attribute_Reference";
   end Attribute_Reference;

   -------------------------------------
   -- Basic_Declarative_Item_Sequence --
   -------------------------------------

   function Basic_Declarative_Item_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------
   -- Block_Statement --
   ---------------------

   function Block_Statement
     (Self               : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token : Node; Declare_Token : Node; Block_Declarative_Items : Node;
      Begin_Token : Node; Block_Statements : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token    : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Block_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Block_Statement";
   end Block_Statement;

   ---------
   -- Box --
   ---------

   function Box (Self : Node_Factory'Class; Box_Token : Node) return Node is
   begin
      pragma Compile_Time_Warning (Standard.True, "Box unimplemented");
      return raise Program_Error with "Unimplemented function Box";
   end Box;

   ---------------------
   -- Case_Expression --
   ---------------------

   function Case_Expression
     (Self     : Node_Factory'Class; Case_Token : Node; Expression : Node;
      Is_Token : Node; Case_Expression_Paths : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Case_Expression unimplemented");
      return raise Program_Error with "Unimplemented function Case_Expression";
   end Case_Expression;

   --------------------------
   -- Case_Expression_Path --
   --------------------------

   function Case_Expression_Path
     (Self                          : Node_Factory'Class; When_Token : Node;
      Case_Path_Alternative_Choices : Node; Arrow_Token : Node;
      Dependent_Expression          : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Case_Expression_Path unimplemented");
      return raise Program_Error
          with "Unimplemented function Case_Expression_Path";
   end Case_Expression_Path;

   -----------------------------------
   -- Case_Expression_Path_Sequence --
   -----------------------------------

   function Case_Expression_Path_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------
   -- Case_Path --
   ---------------

   function Case_Path
     (Self : Node_Factory'Class; When_Token : Node; Variant_Choices : Node;
      Arrow_Token : Node; Sequence_Of_Statements : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Case_Path unimplemented");
      return raise Program_Error with "Unimplemented function Case_Path";
   end Case_Path;

   ------------------------
   -- Case_Path_Sequence --
   ------------------------

   function Case_Path_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------
   -- Case_Statement --
   --------------------

   function Case_Statement
     (Self     : Node_Factory'Class; Case_Token : Node; Case_Expression : Node;
      Is_Token : Node; Case_Statement_Paths : Node; End_Token : Node;
      Endcase  : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Case_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Case_Statement";
   end Case_Statement;

   -----------------------
   -- Character_Literal --
   -----------------------

   function Character_Literal
     (Self : Node_Factory'Class; Character_Literal_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Character_Literal unimplemented");
      return raise Program_Error
          with "Unimplemented function Character_Literal";
   end Character_Literal;

   ------------------------------------
   -- Choice_Parameter_Specification --
   ------------------------------------

   function Choice_Parameter_Specification
     (Self : Node_Factory'Class; Names : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Choice_Parameter_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Choice_Parameter_Specification";
   end Choice_Parameter_Specification;

   -------------------------------
   -- Clause_Or_Pragma_Sequence --
   -------------------------------

   function Clause_Or_Pragma_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   -----------------
   -- Compilation --
   -----------------

   function Compilation
     (Self : Node_Factory'Class; Units : Node; Compilation_Pragmas : Node)
      return Node is
   begin
      return (Compilation_Node, Units.Units, Compilation_Pragmas.Vector);
   end Compilation;

   ---------------------------
   -- Compilation_Unit_Body --
   ---------------------------

   function Compilation_Unit_Body
     (Self             : Node_Factory'Class; Context_Clause_Elements : Node;
      Unit_Declaration : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Compilation_Unit_Body unimplemented");
      return raise Program_Error
          with "Unimplemented function Compilation_Unit_Body";
   end Compilation_Unit_Body;

   ----------------------------------
   -- Compilation_Unit_Declaration --
   ----------------------------------

   type Unit_Declaration_Access is access all
     Program.Units.Declarations.Unit_Declaration
       with Storage_Pool => Program.Storage_Pools.Pool;

   function Compilation_Unit_Declaration
     (Self          : Node_Factory'Class; Context_Clause_Elements : Node;
      Private_Token : Node; Unit_Declaration : Node) return Node
   is
      Result : Unit_Declaration_Access :=
        new (Self.Subpool) Program.Units.Declarations.Unit_Declaration;
      Clause : Program.Element_Vectors.Element_Vector_Access :=
        To_Element_Vector
          (Context_Clause_Elements.Vector'Unchecked_Access, Self.Subpool);
   begin
      Result.Initialize
        (Compilation    => Self.Comp,
         Full_Name      => "",
         Context_Clause => Clause,
         Declaration    => Unit_Declaration.Element,
         Parent         => null);

      return
        (Unit_Node,
         Program.Compilation_Units.Compilation_Unit_Access (Result));
   end Compilation_Unit_Declaration;

   -------------------------------
   -- Compilation_Unit_Sequence --
   -------------------------------

   function Compilation_Unit_Sequence (Self : Node_Factory'Class) return Node
   is
   begin
      return (Unit_Sequence_Node, Unit_Vectors.Empty_Vector);
   end Compilation_Unit_Sequence;

   ----------------------
   -- Component_Clause --
   ----------------------

   function Component_Clause
     (Self : Node_Factory'Class; Representation_Clause_Name : Node;
      At_Token : Node; Component_Clause_Position : Node; Range_Token : Node;
      Component_Clause_Range : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Component_Clause unimplemented");
      return raise Program_Error
          with "Unimplemented function Component_Clause";
   end Component_Clause;

   ---------------------------
   -- Component_Declaration --
   ---------------------------

   function Component_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node; Aspect_Specifications : Node;
      Semicolon_Token            : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Component_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Component_Declaration";
   end Component_Declaration;

   --------------------------
   -- Component_Definition --
   --------------------------

   function Component_Definition
     (Self                         : Node_Factory'Class; Aliased_Token : Node;
      Component_Subtype_Indication : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Component_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Component_Definition";
   end Component_Definition;

   -----------------------------
   -- Component_Item_Sequence --
   -----------------------------

   function Component_Item_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ----------------------------------
   -- Constrained_Array_Definition --
   ----------------------------------

   function Constrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Discrete_Subtype_Definitions : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition   : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Constrained_Array_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Constrained_Array_Definition";
   end Constrained_Array_Definition;

   ---------------------------
   -- Context_Item_Sequence --
   ---------------------------

   function Context_Item_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ------------------------------------
   -- Decimal_Fixed_Point_Definition --
   ------------------------------------

   function Decimal_Fixed_Point_Definition
     (Self : Node_Factory'Class; Delta_Token : Node; Delta_Expression : Node;
      Digits_Token          : Node; Digits_Expression : Node;
      Real_Range_Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Decimal_Fixed_Point_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Decimal_Fixed_Point_Definition";
   end Decimal_Fixed_Point_Definition;

   -------------------------------
   -- Declarative_Item_Sequence --
   -------------------------------

   function Declarative_Item_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------------------
   -- Defining_Character_Literal --
   --------------------------------

   function Defining_Character_Literal
     (Self : Node_Factory'Class; Character_Literal : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Defining_Character_Literal unimplemented");
      return raise Program_Error
          with "Unimplemented function Defining_Character_Literal";
   end Defining_Character_Literal;

   ----------------------------------
   -- Defining_Enumeration_Literal --
   ----------------------------------

   function Defining_Enumeration_Literal
     (Self : Node_Factory'Class; Identifier : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Defining_Identifier (Identifier.Token)));
   end Defining_Enumeration_Literal;

   -------------------------
   -- Defining_Identifier --
   -------------------------

   function Defining_Identifier
     (Self : Node_Factory'Class; Identifier_Token : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Defining_Identifier (Identifier_Token.Token)));
   end Defining_Identifier;

   ----------------------------------
   -- Defining_Identifier_Sequence --
   ----------------------------------

   function Defining_Identifier_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ------------------------------
   -- Defining_Operator_Symbol --
   ------------------------------

   function Defining_Operator_Symbol
     (Self : Node_Factory'Class; Operator_Symbol_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Defining_Operator_Symbol unimplemented");
      return raise Program_Error
          with "Unimplemented function Defining_Operator_Symbol";
   end Defining_Operator_Symbol;

   ---------------------
   -- Delay_Statement --
   ---------------------

   function Delay_Statement
     (Self : Node_Factory'Class; Delay_Token : Node; Until_Token : Node;
      Delay_Expression : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Delay_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Delay_Statement";
   end Delay_Statement;

   ----------------------
   -- Delta_Constraint --
   ----------------------

   function Delta_Constraint
     (Self : Node_Factory'Class; Delta_Token : Node; Delta_Expression : Node;
      Real_Range_Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Delta_Constraint unimplemented");
      return raise Program_Error
          with "Unimplemented function Delta_Constraint";
   end Delta_Constraint;

   -------------------------------
   -- Derived_Record_Definition --
   -------------------------------

   function Derived_Record_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      New_Token       : Node; Parent_Subtype_Indication : Node;
      Progenitor_List : Node; With_Token : Node; Record_Definition : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Derived_Record_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Derived_Record_Definition";
   end Derived_Record_Definition;

   -----------------------------
   -- Derived_Type_Definition --
   -----------------------------

   function Derived_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      New_Token : Node; Parent_Subtype_Indication : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Derived_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Derived_Type_Definition";
   end Derived_Type_Definition;

   -----------------------
   -- Digits_Constraint --
   -----------------------

   function Digits_Constraint
     (Self : Node_Factory'Class; Digits_Token : Node; Digits_Expression : Node;
      Real_Range_Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Digits_Constraint unimplemented");
      return raise Program_Error
          with "Unimplemented function Digits_Constraint";
   end Digits_Constraint;

   ------------------------------
   -- Discrete_Choice_Sequence --
   ------------------------------

   function Discrete_Choice_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ----------------------------------------
   -- Discrete_Range_Attribute_Reference --
   ----------------------------------------

   function Discrete_Range_Attribute_Reference
     (Self : Node_Factory'Class; Range_Attribute : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Discrete_Range_Attribute_Reference unimplemented");
      return raise Program_Error
          with "Unimplemented function Discrete_Range_Attribute_Reference";
   end Discrete_Range_Attribute_Reference;

   --------------------------------------
   -- Discrete_Simple_Expression_Range --
   --------------------------------------

   function Discrete_Simple_Expression_Range
     (Self : Node_Factory'Class; Lower_Bound : Node; Double_Dot_Token : Node;
      Upper_Bound : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Discrete_Simple_Expression_Range unimplemented");
      return raise Program_Error
          with "Unimplemented function Discrete_Simple_Expression_Range";
   end Discrete_Simple_Expression_Range;

   ------------------------------------------
   -- Discrete_Subtype_Definition_Sequence --
   ------------------------------------------

   function Discrete_Subtype_Definition_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------------------
   -- Discrete_Subtype_Indication --
   ---------------------------------

   function Discrete_Subtype_Indication
     (Self               : Node_Factory'Class; Subtype_Mark : Node;
      Subtype_Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Discrete_Subtype_Indication unimplemented");
      return raise Program_Error
          with "Unimplemented function Discrete_Subtype_Indication";
   end Discrete_Subtype_Indication;

   ------------------------------------
   -- Discrete_Subtype_Indication_Dr --
   ------------------------------------

   function Discrete_Subtype_Indication_Dr
     (Self               : Node_Factory'Class; Subtype_Mark : Node;
      Subtype_Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Discrete_Subtype_Indication_Dr unimplemented");
      return raise Program_Error
          with "Unimplemented function Discrete_Subtype_Indication_Dr";
   end Discrete_Subtype_Indication_Dr;

   --------------------------------
   -- Discriminant_Specification --
   --------------------------------

   function Discriminant_Specification
     (Self             : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Not_Token : Node; Null_Token : Node; Object_Declaration_Subtype : Node;
      Assignment_Token : Node; Initialization_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Discriminant_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Discriminant_Specification";
   end Discriminant_Specification;

   -----------------------------------------
   -- Discriminant_Specification_Sequence --
   -----------------------------------------

   function Discriminant_Specification_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ------------------------------------
   -- Element_Iterator_Specification --
   ------------------------------------

   function Element_Iterator_Specification
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Subtype_Indication    : Node; Of_Token : Node; Reverse_Token : Node;
      Iteration_Scheme_Name : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Element_Iterator_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Element_Iterator_Specification";
   end Element_Iterator_Specification;

   --------------------------
   -- Else_Expression_Path --
   --------------------------

   function Else_Expression_Path
     (Self                 : Node_Factory'Class; Else_Token : Node;
      Dependent_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Else_Expression_Path unimplemented");
      return raise Program_Error
          with "Unimplemented function Else_Expression_Path";
   end Else_Expression_Path;

   ---------------
   -- Else_Path --
   ---------------

   function Else_Path
     (Self                   : Node_Factory'Class; Else_Token : Node;
      Sequence_Of_Statements : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Else_Path unimplemented");
      return raise Program_Error with "Unimplemented function Else_Path";
   end Else_Path;

   ---------------------------
   -- Elsif_Expression_Path --
   ---------------------------

   function Elsif_Expression_Path
     (Self                 : Node_Factory'Class; Elsif_Token : Node;
      Condition_Expression : Node; Then_Token : Node;
      Dependent_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Elsif_Expression_Path unimplemented");
      return raise Program_Error
          with "Unimplemented function Elsif_Expression_Path";
   end Elsif_Expression_Path;

   ----------------
   -- Elsif_Path --
   ----------------

   function Elsif_Path
     (Self                   : Node_Factory'Class; Elsif_Token : Node;
      Condition_Expression   : Node; Then_Token : Node;
      Sequence_Of_Statements : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Elsif_Path unimplemented");
      return raise Program_Error with "Unimplemented function Elsif_Path";
   end Elsif_Path;

   ----------------
   -- Entry_Body --
   ----------------

   function Entry_Body
     (Self : Node_Factory'Class; Entry_Token : Node; Names : Node;
      Left_Parenthesis_Token  : Node; Entry_Index_Specification : Node;
      Right_Parenthesis_Token : Node; Lp_Token : Node;
      Parameter_Profile       : Node; Rp_Token : Node; When_Token : Node;
      Entry_Barrier : Node; Is_Token : Node; Body_Declarative_Items : Node;
      Begin_Token : Node; Body_Statements : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token         : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Entry_Body unimplemented");
      return raise Program_Error with "Unimplemented function Entry_Body";
   end Entry_Body;

   -----------------------
   -- Entry_Declaration --
   -----------------------

   function Entry_Declaration
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Entry_Token : Node; Names : Node; Left_Parenthesis_Token : Node;
      Entry_Family_Definition : Node; Right_Parenthesis_Token : Node;
      Lp_Token : Node; Parameter_Profile : Node; Rp_Token : Node;
      Aspect_Specifications   : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Entry_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Entry_Declaration";
   end Entry_Declaration;

   -------------------------------
   -- Entry_Index_Specification --
   -------------------------------

   function Entry_Index_Specification
     (Self     : Node_Factory'Class; For_Token : Node; Names : Node;
      In_Token : Node; Specification_Subtype_Definition : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Entry_Index_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Entry_Index_Specification";
   end Entry_Index_Specification;

   ---------------------------------------
   -- Enumeration_Literal_Specification --
   ---------------------------------------

   function Enumeration_Literal_Specification
     (Self : Node_Factory'Class; Names : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Enumeration_Literal_Specification
             (Program.Elements.Defining_Identifiers.Defining_Identifier_Access
                (Names.Element))));
   end Enumeration_Literal_Specification;

   ------------------------------------------------
   -- Enumeration_Literal_Specification_Sequence --
   ------------------------------------------------

   function Enumeration_Literal_Specification_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------------------
   -- Enumeration_Type_Definition --
   ---------------------------------

   function Enumeration_Type_Definition
     (Self        : Node_Factory'Class;
      Left_Token  : Node;
      Literals    : Node;
      Right_Token : Node) return Node
   is
      List : Program.Elements.Enumeration_Literal_Specifications
        .Enumeration_Literal_Specification_Vector_Access :=
          To_Enumeration_Literal_Specification_Vector
            (Literals.Vector'Unchecked_Access, Self.Subpool);
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Enumeration_Type
                (Left_Token.Token, List, Right_Token.Token)));
   end Enumeration_Type_Definition;

   -------------------------------
   -- Exception_Choice_Sequence --
   -------------------------------

   function Exception_Choice_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------------
   -- Exception_Declaration --
   ---------------------------

   function Exception_Declaration
     (Self            : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Exception_Token : Node; Aspect_Specifications : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Exception_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Exception_Declaration";
   end Exception_Declaration;

   -----------------------
   -- Exception_Handler --
   -----------------------

   function Exception_Handler
     (Self                           : Node_Factory'Class; When_Token : Node;
      Choice_Parameter_Specification : Node; Colon_Token : Node;
      Exception_Choices : Node; Arrow_Token : Node; Handler_Statements : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Exception_Handler unimplemented");
      return raise Program_Error
          with "Unimplemented function Exception_Handler";
   end Exception_Handler;

   --------------------------------
   -- Exception_Handler_Sequence --
   --------------------------------

   function Exception_Handler_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ------------------------------------
   -- Exception_Renaming_Declaration --
   ------------------------------------

   function Exception_Renaming_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Exception_Token : Node; Renames_Token : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Exception_Renaming_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Exception_Renaming_Declaration";
   end Exception_Renaming_Declaration;

   --------------------
   -- Exit_Statement --
   --------------------

   function Exit_Statement
     (Self : Node_Factory'Class; Exit_Token : Node; Exit_Loop_Name : Node;
      When_Token : Node; Exit_Condition : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Exit_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Exit_Statement";
   end Exit_Statement;

   --------------------------
   -- Explicit_Dereference --
   --------------------------

   function Explicit_Dereference
     (Self      : Node_Factory'Class; Prefix : Node; Dot_Token : Node;
      All_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Explicit_Dereference unimplemented");
      return raise Program_Error
          with "Unimplemented function Explicit_Dereference";
   end Explicit_Dereference;

   -------------------------------
   -- Extended_Return_Statement --
   -------------------------------

   function Extended_Return_Statement
     (Self                        : Node_Factory'Class; Return_Token : Node;
      Return_Object_Specification : Node; Do_Token : Node;
      Extended_Return_Statements  : Node; Exception_Token : Node;
      Exception_Handlers          : Node; End_Token : Node; End_Return : Node;
      Semicolon_Token             : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Extended_Return_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function Extended_Return_Statement";
   end Extended_Return_Statement;

   -------------------------
   -- Extension_Aggregate --
   -------------------------

   function Extension_Aggregate
     (Self                           : Node_Factory'Class; Left_Token : Node;
      Extension_Aggregate_Expression : Node; With_Token : Node;
      Record_Component_Associations  : Node; Right_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Extension_Aggregate unimplemented");
      return raise Program_Error
          with "Unimplemented function Extension_Aggregate";
   end Extension_Aggregate;

   -------------------------------
   -- Floating_Point_Definition --
   -------------------------------

   function Floating_Point_Definition
     (Self : Node_Factory'Class; Digits_Token : Node; Digits_Expression : Node;
      Real_Range_Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Floating_Point_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Floating_Point_Definition";
   end Floating_Point_Definition;

   ------------------------
   -- For_Loop_Statement --
   ------------------------

   function For_Loop_Statement
     (Self : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token                  : Node; For_Token : Node;
      Loop_Parameter_Specification : Node; Loop_Token : Node;
      Loop_Statements              : Node; End_Token : Node; End_Loop : Node;
      Identifier_Token             : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "For_Loop_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function For_Loop_Statement";
   end For_Loop_Statement;

   ------------------------------------------
   -- Formal_Access_To_Function_Definition --
   ------------------------------------------

   function Formal_Access_To_Function_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token      : Node; Protected_Token : Node; Function_Token : Node;
      Lp_Token          : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token          : Node; Return_Token : Node; Return_Not_Token : Node;
      Return_Null_Token : Node; Access_To_Function_Result_Subtype : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Access_To_Function_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Access_To_Function_Definition";
   end Formal_Access_To_Function_Definition;

   ----------------------------------------
   -- Formal_Access_To_Object_Definition --
   ----------------------------------------

   function Formal_Access_To_Object_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Constant_Token : Node; Subtype_Indication : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Access_To_Object_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Access_To_Object_Definition";
   end Formal_Access_To_Object_Definition;

   -------------------------------------------
   -- Formal_Access_To_Procedure_Definition --
   -------------------------------------------

   function Formal_Access_To_Procedure_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Protected_Token : Node; Procedure_Token : Node;
      Lp_Token     : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token     : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Access_To_Procedure_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Access_To_Procedure_Definition";
   end Formal_Access_To_Procedure_Definition;

   -----------------------------------------
   -- Formal_Constrained_Array_Definition --
   -----------------------------------------

   function Formal_Constrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Discrete_Subtype_Definitions : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition   : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Constrained_Array_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Constrained_Array_Definition";
   end Formal_Constrained_Array_Definition;

   -------------------------------------------
   -- Formal_Decimal_Fixed_Point_Definition --
   -------------------------------------------

   function Formal_Decimal_Fixed_Point_Definition
     (Self         : Node_Factory'Class; Delta_Token : Node; Delta_Box : Node;
      Digits_Token : Node; Digits_Box : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Decimal_Fixed_Point_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Decimal_Fixed_Point_Definition";
   end Formal_Decimal_Fixed_Point_Definition;

   ------------------------------------
   -- Formal_Derived_Type_Definition --
   ------------------------------------

   function Formal_Derived_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      Synchronized_Token : Node; New_Token : Node; Subtype_Mark : Node;
      And_Token          : Node; Progenitor_List : Node; With_Token : Node;
      Private_Token      : Node; Aspect_Specifications : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Derived_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Derived_Type_Definition";
   end Formal_Derived_Type_Definition;

   -------------------------------------
   -- Formal_Discrete_Type_Definition --
   -------------------------------------

   function Formal_Discrete_Type_Definition
     (Self      : Node_Factory'Class; Left_Parenthesis_Token : Node;
      Box_Token : Node; Right_Parenthesis_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Discrete_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Discrete_Type_Definition";
   end Formal_Discrete_Type_Definition;

   --------------------------------------
   -- Formal_Floating_Point_Definition --
   --------------------------------------

   function Formal_Floating_Point_Definition
     (Self : Node_Factory'Class; Digits_Token : Node; Box_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Floating_Point_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Floating_Point_Definition";
   end Formal_Floating_Point_Definition;

   ---------------------------------
   -- Formal_Function_Declaration --
   ---------------------------------

   function Formal_Function_Declaration
     (Self : Node_Factory'Class; With_Token : Node; Function_Token : Node;
      Names : Node; Lp_Token : Node; Parameter_Profile : Node; Rp_Token : Node;
      Return_Token : Node; Return_Not_Token : Node; Return_Null_Token : Node;
      Result_Subtype            : Node; Is_Token : Node; Abstract_Token : Node;
      Formal_Subprogram_Default : Node; Box_Token : Node;
      Aspect_Specifications     : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Function_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Function_Declaration";
   end Formal_Function_Declaration;

   ----------------------------------------
   -- Formal_Incomplete_Type_Declaration --
   ----------------------------------------

   function Formal_Incomplete_Type_Declaration
     (Self              : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Tagged_Token : Node;
      Semicolon_Token   : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Incomplete_Type_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Incomplete_Type_Declaration";
   end Formal_Incomplete_Type_Declaration;

   --------------------------------------
   -- Formal_Interface_Type_Definition --
   --------------------------------------

   function Formal_Interface_Type_Definition
     (Self : Node_Factory'Class; Kind_Token : Node; Interface_Token : Node;
      Progenitor_List : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Interface_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Interface_Type_Definition";
   end Formal_Interface_Type_Definition;

   ------------------------------------
   -- Formal_Modular_Type_Definition --
   ------------------------------------

   function Formal_Modular_Type_Definition
     (Self : Node_Factory'Class; Mod_Token : Node; Box_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Modular_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Modular_Type_Definition";
   end Formal_Modular_Type_Definition;

   -------------------------------
   -- Formal_Object_Declaration --
   -------------------------------

   function Formal_Object_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      In_Token : Node; Out_Token : Node; Not_Token : Node; Null_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node; Aspect_Specifications : Node;
      Semicolon_Token            : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Object_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Object_Declaration";
   end Formal_Object_Declaration;

   --------------------------------------------
   -- Formal_Ordinary_Fixed_Point_Definition --
   --------------------------------------------

   function Formal_Ordinary_Fixed_Point_Definition
     (Self : Node_Factory'Class; Delta_Token : Node; Box_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Ordinary_Fixed_Point_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Ordinary_Fixed_Point_Definition";
   end Formal_Ordinary_Fixed_Point_Definition;

   --------------------------------
   -- Formal_Package_Declaration --
   --------------------------------

   function Formal_Package_Declaration
     (Self : Node_Factory'Class; With_Token : Node; Package_Token : Node;
      Names                 : Node; Is_Token : Node; New_Token : Node;
      Generic_Unit_Name     : Node; Left_Parenthesis_Token : Node;
      Generic_Actual_Part   : Node; Right_Parenthesis_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Package_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Package_Declaration";
   end Formal_Package_Declaration;

   ------------------------------------
   -- Formal_Private_Type_Definition --
   ------------------------------------

   function Formal_Private_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Tagged_Token : Node;
      Limited_Token : Node; Private_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Private_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Private_Type_Definition";
   end Formal_Private_Type_Definition;

   ----------------------------------
   -- Formal_Procedure_Declaration --
   ----------------------------------

   function Formal_Procedure_Declaration
     (Self : Node_Factory'Class; With_Token : Node; Procedure_Token : Node;
      Names : Node; Lp_Token : Node; Parameter_Profile : Node; Rp_Token : Node;
      Is_Token              : Node; Abstract_Token : Node; Box_Token : Node;
      Null_Token            : Node; Formal_Subprogram_Default : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Procedure_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Procedure_Declaration";
   end Formal_Procedure_Declaration;

   -------------------------------------------
   -- Formal_Signed_Integer_Type_Definition --
   -------------------------------------------

   function Formal_Signed_Integer_Type_Definition
     (Self : Node_Factory'Class; Range_Token : Node; Box_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Signed_Integer_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Signed_Integer_Type_Definition";
   end Formal_Signed_Integer_Type_Definition;

   -----------------------------
   -- Formal_Type_Declaration --
   -----------------------------

   function Formal_Type_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Type_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Type_Declaration";
   end Formal_Type_Declaration;

   -------------------------------------------
   -- Formal_Unconstrained_Array_Definition --
   -------------------------------------------

   function Formal_Unconstrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Index_Subtype_Definitions  : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Formal_Unconstrained_Array_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Formal_Unconstrained_Array_Definition";
   end Formal_Unconstrained_Array_Definition;

   ---------------------------
   -- Full_Type_Declaration --
   ---------------------------

   function Full_Type_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Type_Declaration
                (Type_Token        => Type_Token.Token,
                 Name              => Program.Elements.Defining_Identifiers
                    .Defining_Identifier_Access (Names.Element),
                 Discriminant_Part => Program.Elements.Definitions
                    .Definition_Access (Discriminant_Part.Element),
                 Is_Token          => Is_Token.Token,
                 Definition        => Program.Elements.Definitions
                    .Definition_Access (Type_Declaration_View.Element),
                 With_Token        => null,  --  FIXME
                 Aspects           => To_Aspect_Specification_Vector
                   (Aspect_Specifications.Vector'Unchecked_Access,
                    Self.Subpool),
                 Semicolon_Token   => Semicolon_Token.Token)));
   end Full_Type_Declaration;

   -------------------
   -- Function_Body --
   -------------------

   function Function_Body
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Function_Token         : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile      : Node; Rp_Token : Node; Return_Token : Node;
      Return_Not_Token : Node; Return_Null_Token : Node; Result_Subtype : Node;
      Aspect_Specifications  : Node; Is_Token : Node;
      Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements        : Node; Exception_Token : Node;
      Exception_Handlers     : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token        : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Function_Body unimplemented");
      return raise Program_Error with "Unimplemented function Function_Body";
   end Function_Body;

   -------------------
   -- Function_Call --
   -------------------

   function Function_Call
     (Self                     : Node_Factory'Class; Prefix : Node;
      Function_Call_Parameters : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Function_Call unimplemented");
      return raise Program_Error with "Unimplemented function Function_Call";
   end Function_Call;

   --------------------------
   -- Function_Declaration --
   --------------------------

   function Function_Declaration
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Function_Token        : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile     : Node; Rp_Token : Node; Return_Token : Node;
      Return_Not_Token : Node; Return_Null_Token : Node; Result_Subtype : Node;
      Is_Token : Node; Abstract_Token : Node; Result_Expression : Node;
      Renames_Token : Node; Renamed_Entity : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Function_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Function_Declaration";
   end Function_Declaration;

   ----------------------------
   -- Function_Instantiation --
   ----------------------------

   function Function_Instantiation
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Function_Token : Node; Names : Node; Is_Token : Node; New_Token : Node;
      Generic_Unit_Name     : Node; Left_Parenthesis_Token : Node;
      Generic_Actual_Part   : Node; Right_Parenthesis_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Function_Instantiation unimplemented");
      return raise Program_Error
          with "Unimplemented function Function_Instantiation";
   end Function_Instantiation;

   -------------------------
   -- Generic_Association --
   -------------------------

   function Generic_Association
     (Self : Node_Factory'Class; Formal_Parameter : Node; Arrow_Token : Node;
      Actual_Parameter : Node; Box_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Generic_Association unimplemented");
      return raise Program_Error
          with "Unimplemented function Generic_Association";
   end Generic_Association;

   ----------------------------------
   -- Generic_Association_Sequence --
   ----------------------------------

   function Generic_Association_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   -----------------------------
   -- Generic_Formal_Sequence --
   -----------------------------

   function Generic_Formal_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ----------------------------------
   -- Generic_Function_Declaration --
   ----------------------------------

   function Generic_Function_Declaration
     (Self                : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part : Node; Function_Token : Node; Names : Node;
      Lp_Token            : Node; Parameter_Profile : Node; Rp_Token : Node;
      Return_Token : Node; Return_Not_Token : Node; Return_Null_Token : Node;
      Result_Subtype      : Node; Aspect_Specifications : Node;
      Semicolon_Token     : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Generic_Function_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Generic_Function_Declaration";
   end Generic_Function_Declaration;

   -------------------------------
   -- Generic_Function_Renaming --
   -------------------------------

   function Generic_Function_Renaming
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Function_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Generic_Function_Renaming unimplemented");
      return raise Program_Error
          with "Unimplemented function Generic_Function_Renaming";
   end Generic_Function_Renaming;

   ---------------------------------
   -- Generic_Package_Declaration --
   ---------------------------------

   function Generic_Package_Declaration
     (Self : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part : Node; Package_Token : Node; Names : Node;
      Aspect_Specifications          : Node; Is_Token : Node;
      Visible_Part_Declarative_Items : Node; Private_Token : Node;
      Private_Part_Declarative_Items : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token                : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Generic_Package_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Generic_Package_Declaration";
   end Generic_Package_Declaration;

   ------------------------------
   -- Generic_Package_Renaming --
   ------------------------------

   function Generic_Package_Renaming
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Package_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Generic_Package_Renaming unimplemented");
      return raise Program_Error
          with "Unimplemented function Generic_Package_Renaming";
   end Generic_Package_Renaming;

   -----------------------------------
   -- Generic_Procedure_Declaration --
   -----------------------------------

   function Generic_Procedure_Declaration
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Procedure_Token : Node; Names : Node;
      Lp_Token              : Node; Parameter_Profile : Node; Rp_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Generic_Procedure_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Generic_Procedure_Declaration";
   end Generic_Procedure_Declaration;

   --------------------------------
   -- Generic_Procedure_Renaming --
   --------------------------------

   function Generic_Procedure_Renaming
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Procedure_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Generic_Procedure_Renaming unimplemented");
      return raise Program_Error
          with "Unimplemented function Generic_Procedure_Renaming";
   end Generic_Procedure_Renaming;

   ---------------------------
   -- Get_Compilation_Units --
   ---------------------------

   procedure Get_Compilation_Units
     (Value   : Node;
      Units   : out Program.Parsers.Unit_Vectors.Vector;
      Pragmas : out Program.Parsers.Element_Vectors.Vector) is
   begin
      Units := Value.Root_Units;
      Pragmas := Value.Pragmas;
   end Get_Compilation_Units;

   --------------------
   -- Goto_Statement --
   --------------------

   function Goto_Statement
     (Self : Node_Factory'Class; Exit_Token : Node; Goto_Label : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Goto_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Goto_Statement";
   end Goto_Statement;

   ----------------
   -- Identifier --
   ----------------

   function Identifier
     (Self : Node_Factory'Class; Identifier_Token : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Identifier (Identifier_Token.Token)));
   end Identifier;

   --------------------------------------
   -- If_Else_Expression_Path_Sequence --
   --------------------------------------

   function If_Else_Expression_Path_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------------------
   -- If_Elsif_Else_Path_Sequence --
   ---------------------------------

   function If_Elsif_Else_Path_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   -------------------
   -- If_Expression --
   -------------------

   function If_Expression
     (Self : Node_Factory'Class; Expression_Paths : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "If_Expression unimplemented");
      return raise Program_Error with "Unimplemented function If_Expression";
   end If_Expression;

   ------------------------
   -- If_Expression_Path --
   ------------------------

   function If_Expression_Path
     (Self : Node_Factory'Class; If_Token : Node; Condition_Expression : Node;
      Then_Token : Node; Dependent_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "If_Expression_Path unimplemented");
      return raise Program_Error
          with "Unimplemented function If_Expression_Path";
   end If_Expression_Path;

   -------------
   -- If_Path --
   -------------

   function If_Path
     (Self : Node_Factory'Class; If_Token : Node; Condition_Expression : Node;
      Then_Token : Node; Sequence_Of_Statements : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "If_Path unimplemented");
      return raise Program_Error with "Unimplemented function If_Path";
   end If_Path;

   ------------------
   -- If_Statement --
   ------------------

   function If_Statement
     (Self     : Node_Factory'Class; Statement_Paths : Node; End_Token : Node;
      If_Token : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "If_Statement unimplemented");
      return raise Program_Error with "Unimplemented function If_Statement";
   end If_Statement;

   ---------------------------------
   -- Incomplete_Type_Declaration --
   ---------------------------------

   function Incomplete_Type_Declaration
     (Self              : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Semicolon_Token   : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Incomplete_Type_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Incomplete_Type_Declaration";
   end Incomplete_Type_Declaration;

   --------------------------------
   -- Incomplete_Type_Definition --
   --------------------------------

   function Incomplete_Type_Definition
     (Self : Node_Factory'Class; Tagged_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Incomplete_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Incomplete_Type_Definition";
   end Incomplete_Type_Definition;

   ----------------
   -- Infix_Call --
   ----------------

   function Infix_Call
     (Self : Node_Factory'Class; Prefix, Left, Right : Node) return Node
   is
      Operator : Program.Elements.Operator_Symbols.Operator_Symbol_Access :=
        Self.EF.Create_Operator_Symbol (Prefix.Token);
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Infix_Operator
                (Left     => Program.Elements.Expressions.Expression_Access
                               (Left.Element),
                 Operator => Operator,
                 Right    => Program.Elements.Expressions.Expression_Access
                               (Right.Element))));
   end Infix_Call;

   -------------------------------
   -- Interface_Type_Definition --
   -------------------------------

   function Interface_Type_Definition
     (Self : Node_Factory'Class; Kind_Token : Node; Interface_Token : Node;
      Progenitor_List : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Interface_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Interface_Type_Definition";
   end Interface_Type_Definition;

   -----------------------------
   -- Known_Discriminant_Part --
   -----------------------------

   function Known_Discriminant_Part
     (Self          : Node_Factory'Class; Left_Parenthesis_Token : Node;
      Discriminants : Node; Right_Parenthesis_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Known_Discriminant_Part unimplemented");
      return raise Program_Error
          with "Unimplemented function Known_Discriminant_Part";
   end Known_Discriminant_Part;

   ---------------------
   -- Label_Decorator --
   ---------------------

   function Label_Decorator
     (Self                : Node_Factory'Class; Label_Names : Node;
      Unlabeled_Statement : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Label_Decorator unimplemented");
      return raise Program_Error with "Unimplemented function Label_Decorator";
   end Label_Decorator;

   ----------------------------------
   -- Loop_Parameter_Specification --
   ----------------------------------

   function Loop_Parameter_Specification
     (Self          : Node_Factory'Class; Names : Node; In_Token : Node;
      Reverse_Token : Node; Specification_Subtype_Definition : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Loop_Parameter_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Loop_Parameter_Specification";
   end Loop_Parameter_Specification;

   --------------------
   -- Loop_Statement --
   --------------------

   function Loop_Statement
     (Self            : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token     : Node; Loop_Token : Node; Loop_Statements : Node;
      End_Token       : Node; End_Loop : Node; Identifier_Token : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Loop_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Loop_Statement";
   end Loop_Statement;

   --------------------------------
   -- Membership_Choice_Sequence --
   --------------------------------

   function Membership_Choice_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------
   -- Membership_Test --
   ---------------------

   function Membership_Test
     (Self      : Node_Factory'Class; Membership_Test_Expression : Node;
      Not_Token : Node; In_Token : Node; Membership_Test_Choices : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Membership_Test unimplemented");
      return raise Program_Error with "Unimplemented function Membership_Test";
   end Membership_Test;

   -----------------------------
   -- Modular_Type_Definition --
   -----------------------------

   function Modular_Type_Definition
     (Self                  : Node_Factory'Class; Mod_Token : Node;
      Mod_Static_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Modular_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Modular_Type_Definition";
   end Modular_Type_Definition;

   --------------------------
   -- New_Element_Sequence --
   --------------------------

   function New_Element_Sequence (Self : Node_Factory'Class) return Node is
   begin
      return (Element_Sequence_Node,   Element_Vectors.Empty_Vector);
   end New_Element_Sequence;

   -------------------
   -- Name_Sequence --
   -------------------

   function Name_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------
   -- Null_Component --
   --------------------

   function Null_Component
     (Self : Node_Factory'Class; Null_Token : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Null_Component unimplemented");
      return raise Program_Error with "Unimplemented function Null_Component";
   end Null_Component;

   ------------------
   -- Null_Literal --
   ------------------

   function Null_Literal
     (Self : Node_Factory'Class; Null_Literal_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Null_Literal unimplemented");
      return raise Program_Error with "Unimplemented function Null_Literal";
   end Null_Literal;

   ----------------------------
   -- Null_Record_Definition --
   ----------------------------

   function Null_Record_Definition
     (Self : Node_Factory'Class; Null_Token : Node; Record_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Null_Record_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Null_Record_Definition";
   end Null_Record_Definition;

   --------------------
   -- Null_Statement --
   --------------------

   function Null_Statement
     (Self : Node_Factory'Class; Null_Token : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Null_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Null_Statement";
   end Null_Statement;

   ------------------------
   -- Number_Declaration --
   ------------------------

   function Number_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Constant_Token            : Node; Assignment_Token : Node;
      Initialization_Expression : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Number_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Number_Declaration";
   end Number_Declaration;

   ---------------------
   -- Numeric_Literal --
   ---------------------

   function Numeric_Literal
     (Self : Node_Factory'Class; Numeric_Literal_Token : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Numeric_Literal (Numeric_Literal_Token.Token)));
   end Numeric_Literal;

   ------------------------
   -- Object_Declaration --
   ------------------------

   function Object_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Aliased_Token              : Node; Constant_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node; Aspect_Specifications : Node;
      Semicolon_Token            : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Object_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Object_Declaration";
   end Object_Declaration;

   ---------------------------------
   -- Object_Renaming_Declaration --
   ---------------------------------

   function Object_Renaming_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Not_Token : Node; Null_Token : Node; Object_Declaration_Subtype : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Object_Renaming_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Object_Renaming_Declaration";
   end Object_Renaming_Declaration;

   ---------------------
   -- Operator_Symbol --
   ---------------------

   function Operator_Symbol
     (Self : Node_Factory'Class; Operator_Symbol_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Operator_Symbol unimplemented");
      return raise Program_Error with "Unimplemented function Operator_Symbol";
   end Operator_Symbol;

   -------------------------------------
   -- Ordinary_Fixed_Point_Definition --
   -------------------------------------

   function Ordinary_Fixed_Point_Definition
     (Self : Node_Factory'Class; Delta_Token : Node; Delta_Expression : Node;
      Real_Range_Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Ordinary_Fixed_Point_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Ordinary_Fixed_Point_Definition";
   end Ordinary_Fixed_Point_Definition;

   -------------------
   -- Others_Choice --
   -------------------

   function Others_Choice
     (Self : Node_Factory'Class; Others_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Others_Choice unimplemented");
      return raise Program_Error with "Unimplemented function Others_Choice";
   end Others_Choice;

   ------------------
   -- Package_Body --
   ------------------

   function Package_Body
     (Self : Node_Factory'Class; Package_Token : Node; Body_Token : Node;
      Names : Node; Aspect_Specifications : Node; Is_Token : Node;
      Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements        : Node; Exception_Token : Node;
      Exception_Handlers     : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token        : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Package_Body unimplemented");
      return raise Program_Error with "Unimplemented function Package_Body";
   end Package_Body;

   -----------------------
   -- Package_Body_Stub --
   -----------------------

   function Package_Body_Stub
     (Self : Node_Factory'Class; Package_Token : Node; Body_Token : Node;
      Names                 : Node; Is_Token : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Package_Body_Stub unimplemented");
      return raise Program_Error
          with "Unimplemented function Package_Body_Stub";
   end Package_Body_Stub;

   -------------------------
   -- Package_Declaration --
   -------------------------

   function Package_Declaration
     (Self : Node_Factory'Class; Package_Token : Node; Names : Node;
      Aspect_Specifications          : Node; Is_Token : Node;
      Visible_Part_Declarative_Items : Node; Private_Token : Node;
      Private_Part_Declarative_Items : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token                : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Package_Declaration
                (Package_Token        => Package_Token.Token,
                 Name                 =>
                   Program.Elements.Defining_Names.Defining_Name_Access
                     (Names.Element),
                 With_Token           => null,
                 Aspects              => To_Aspect_Specification_Vector
                   (Aspect_Specifications.Vector'Unchecked_Access,
                    Self.Subpool),
                 Is_Token             => Is_Token.Token,
                 Visible_Declarations => null,
                 Private_Token        => Private_Token.Token,
                 Private_Declarations => null,
                 End_Token            => End_Token.Token,
                 End_Name             =>
                   Program.Elements.Expressions.Expression_Access
                     (End_Name.Element),
                 Semicolon_Token      => Semicolon_Token.Token)));
   end Package_Declaration;

   ---------------------------
   -- Package_Instantiation --
   ---------------------------

   function Package_Instantiation
     (Self : Node_Factory'Class; Package_Token : Node; Names : Node;
      Is_Token : Node; New_Token : Node; Generic_Unit_Name : Node;
      Left_Parenthesis_Token  : Node; Generic_Actual_Part : Node;
      Right_Parenthesis_Token : Node; Aspect_Specifications : Node;
      Semicolon_Token         : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Package_Instantiation unimplemented");
      return raise Program_Error
          with "Unimplemented function Package_Instantiation";
   end Package_Instantiation;

   ----------------------------------
   -- Package_Renaming_Declaration --
   ----------------------------------

   function Package_Renaming_Declaration
     (Self : Node_Factory'Class; Package_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Package_Renaming_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Package_Renaming_Declaration";
   end Package_Renaming_Declaration;

   -----------------------------
   -- Parameter_Specification --
   -----------------------------

   function Parameter_Specification
     (Self             : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Aliased_Token    : Node; In_Token : Node; Out_Token : Node;
      Not_Token : Node; Null_Token : Node; Object_Declaration_Subtype : Node;
      Assignment_Token : Node; Initialization_Expression : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Parameter_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Parameter_Specification";
   end Parameter_Specification;

   --------------------------------------
   -- Parameter_Specification_Sequence --
   --------------------------------------

   function Parameter_Specification_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------------------
   -- Pragma_Argument_Association --
   ---------------------------------

   function Pragma_Argument_Association
     (Self : Node_Factory'Class; Formal_Parameter : Node; Arrow_Token : Node;
      Actual_Parameter : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Pragma_Argument_Association unimplemented");
      return raise Program_Error
          with "Unimplemented function Pragma_Argument_Association";
   end Pragma_Argument_Association;

   ------------------------------------------
   -- Pragma_Argument_Association_Sequence --
   ------------------------------------------

   function Pragma_Argument_Association_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   -----------------
   -- Pragma_Node --
   -----------------

   function Pragma_Node
     (Self : Node_Factory'Class; Pragma_Token : Node; Formal_Parameter : Node;
      Left_Token  : Node; Pragma_Argument_Associations : Node;
      Right_Token : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Pragma_Node unimplemented");
      return raise Program_Error with "Unimplemented function Pragma_Node";
   end Pragma_Node;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self : Node_Factory'Class;
      List : in out Node;
      Item : Node) is
   begin
      List.Vector.Prepend (Item.Element);
   end Prepend;

   ----------------------------------
   -- Prepend_Aspect_Specification --
   ----------------------------------

   procedure Prepend_Aspect_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -------------------------
   -- Prepend_Association --
   -------------------------

   procedure Prepend_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ----------------------------------
   -- Prepend_Case_Expression_Path --
   ----------------------------------

   procedure Prepend_Case_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------
   -- Prepend_Case_Path --
   -----------------------

   procedure Prepend_Case_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ------------------------------
   -- Prepend_Compilation_Unit --
   ------------------------------

   procedure Prepend_Compilation_Unit
     (Self : Node_Factory'Class; List : in out Node; Item : Node) is
   begin
      List.Units.Prepend (Item.Unit);
   end Prepend_Compilation_Unit;

   ----------------------------
   -- Prepend_Component_Item --
   ----------------------------

   procedure Prepend_Component_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ------------------------------
   -- Prepend_Declarative_Item --
   ------------------------------

   procedure Prepend_Declarative_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ---------------------------------
   -- Prepend_Defining_Identifier --
   ---------------------------------

   procedure Prepend_Defining_Identifier
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------------
   -- Prepend_Discrete_Choice --
   -----------------------------

   procedure Prepend_Discrete_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------------------------
   -- Prepend_Discrete_Subtype_Definition --
   -----------------------------------------

   procedure Prepend_Discrete_Subtype_Definition
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ----------------------------------------
   -- Prepend_Discriminant_Specification --
   ----------------------------------------

   procedure Prepend_Discriminant_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------------------------------
   -- Prepend_Enumeration_Literal_Specification --
   -----------------------------------------------

   procedure Prepend_Enumeration_Literal_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ------------------------------
   -- Prepend_Exception_Choice --
   ------------------------------

   procedure Prepend_Exception_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -------------------------------
   -- Prepend_Exception_Handler --
   -------------------------------

   procedure Prepend_Exception_Handler
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ---------------------------------
   -- Prepend_Generic_Association --
   ---------------------------------

   procedure Prepend_Generic_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -------------------------------------
   -- Prepend_If_Else_Expression_Path --
   -------------------------------------

   procedure Prepend_If_Else_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   --------------------------------
   -- Prepend_If_Elsif_Else_Path --
   --------------------------------

   procedure Prepend_If_Elsif_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -------------------------------
   -- Prepend_Membership_Choice --
   -------------------------------

   procedure Prepend_Membership_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ------------------
   -- Prepend_Name --
   ------------------

   procedure Prepend_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
      renames Prepend;

   -------------------------------------
   -- Prepend_Parameter_Specification --
   -------------------------------------

   procedure Prepend_Parameter_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------------------------
   -- Prepend_Pragma_Argument_Association --
   -----------------------------------------

   procedure Prepend_Pragma_Argument_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -------------------------------
   -- Prepend_Program_Unit_Name --
   -------------------------------

   procedure Prepend_Program_Unit_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ---------------------------------
   -- Prepend_Select_Or_Else_Path --
   ---------------------------------

   procedure Prepend_Select_Or_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------
   -- Prepend_Statement --
   -----------------------

   procedure Prepend_Statement
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   --------------------------
   -- Prepend_Subtype_Mark --
   --------------------------

   procedure Prepend_Subtype_Mark
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------
   -- Prepend_Task_Item --
   -----------------------

   procedure Prepend_Task_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   ---------------------
   -- Prepend_Variant --
   ---------------------

   procedure Prepend_Variant
     (Self : Node_Factory'Class; List : in out Node; Item : Node)
     renames Prepend;

   -----------------------------------
   -- Private_Extension_Declaration --
   -----------------------------------

   function Private_Extension_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Private_Extension_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Private_Extension_Declaration";
   end Private_Extension_Declaration;

   ----------------------------------
   -- Private_Extension_Definition --
   ----------------------------------

   function Private_Extension_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      Synchronized_Token          : Node; New_Token : Node;
      Ancestor_Subtype_Indication : Node; Progenitor_List : Node;
      With_Token                  : Node; Private_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Private_Extension_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Private_Extension_Definition";
   end Private_Extension_Definition;

   ------------------------------
   -- Private_Type_Declaration --
   ------------------------------

   function Private_Type_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Private_Type_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Private_Type_Declaration";
   end Private_Type_Declaration;

   -----------------------------
   -- Private_Type_Definition --
   -----------------------------

   function Private_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Tagged_Token : Node;
      Limited_Token : Node; Private_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Private_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Private_Type_Definition";
   end Private_Type_Definition;

   --------------------
   -- Procedure_Body --
   --------------------

   function Procedure_Body
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Procedure_Token    : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile  : Node; Rp_Token : Node; Aspect_Specifications : Node;
      Is_Token : Node; Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements    : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token    : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Procedure_Body unimplemented");
      return raise Program_Error with "Unimplemented function Procedure_Body";
   end Procedure_Body;

   ------------------------------
   -- Procedure_Call_Statement --
   ------------------------------

   function Procedure_Call_Statement
     (Self : Node_Factory'Class; Function_Call : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Procedure_Call_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function Procedure_Call_Statement";
   end Procedure_Call_Statement;

   ---------------------------
   -- Procedure_Declaration --
   ---------------------------

   function Procedure_Declaration
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Procedure_Token   : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile : Node; Rp_Token : Node; Is_Token : Node;
      Abstract_Token    : Node; Renames_Token : Node; Renamed_Entity : Node;
      Separate_Token    : Node; Aspect_Specifications : Node;
      Semicolon_Token   : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Procedure_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Procedure_Declaration";
   end Procedure_Declaration;

   -----------------------------
   -- Procedure_Instantiation --
   -----------------------------

   function Procedure_Instantiation
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Procedure_Token : Node; Names : Node; Is_Token : Node; New_Token : Node;
      Generic_Unit_Name     : Node; Left_Parenthesis_Token : Node;
      Generic_Actual_Part   : Node; Right_Parenthesis_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Procedure_Instantiation unimplemented");
      return raise Program_Error
          with "Unimplemented function Procedure_Instantiation";
   end Procedure_Instantiation;

   --------------------------------
   -- Program_Unit_Name_Sequence --
   --------------------------------

   function Program_Unit_Name_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------
   -- Protected_Body --
   --------------------

   function Protected_Body
     (Self : Node_Factory'Class; Protected_Token : Node; Body_Token : Node;
      Names : Node; Aspect_Specifications : Node; Is_Token : Node;
      Protected_Operation_Items : Node; End_Token : Node;
      Identifier_Token          : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Protected_Body unimplemented");
      return raise Program_Error with "Unimplemented function Protected_Body";
   end Protected_Body;

   -------------------------
   -- Protected_Body_Stub --
   -------------------------

   function Protected_Body_Stub
     (Self : Node_Factory'Class; Protected_Token : Node; Body_Token : Node;
      Names                 : Node; Is_Token : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Protected_Body_Stub unimplemented");
      return raise Program_Error
          with "Unimplemented function Protected_Body_Stub";
   end Protected_Body_Stub;

   --------------------------
   -- Protected_Definition --
   --------------------------

   function Protected_Definition
     (Self             : Node_Factory'Class; Visible_Protected_Items : Node;
      Private_Token : Node; Private_Protected_Items : Node; End_Token : Node;
      Identifier_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Protected_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Protected_Definition";
   end Protected_Definition;

   --------------------------------------------
   -- Protected_Element_Declaration_Sequence --
   --------------------------------------------

   function Protected_Element_Declaration_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ----------------------------------------------
   -- Protected_Operation_Declaration_Sequence --
   ----------------------------------------------

   function Protected_Operation_Declaration_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------------------------
   -- Protected_Operation_Item_Sequence --
   ---------------------------------------

   function Protected_Operation_Item_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------------------
   -- Protected_Type_Declaration --
   --------------------------------

   function Protected_Type_Declaration
     (Self : Node_Factory'Class; Protected_Token : Node; Type_Token : Node;
      Names : Node; Discriminant_Part : Node; Aspect_Specifications : Node;
      Is_Token   : Node; New_Token : Node; Progenitor_List : Node;
      With_Token : Node; Type_Declaration_View : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Protected_Type_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Protected_Type_Declaration";
   end Protected_Type_Declaration;

   --------------------------
   -- Qualified_Expression --
   --------------------------

   function Qualified_Expression
     (Self : Node_Factory'Class; Converted_Or_Qualified_Subtype_Mark : Node;
      Apostrophe_Token                  : Node; Left_Parenthesis_Token : Node;
      Converted_Or_Qualified_Expression : Node; Right_Parenthesis_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Qualified_Expression unimplemented");
      return raise Program_Error
          with "Unimplemented function Qualified_Expression";
   end Qualified_Expression;

   ---------------------------
   -- Quantified_Expression --
   ---------------------------

   function Quantified_Expression
     (Self : Node_Factory'Class; For_Token : Node; Quantifier_Token : Node;
      Iterator_Specification : Node; Arrow_Token : Node; Predicate : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Quantified_Expression unimplemented");
      return raise Program_Error
          with "Unimplemented function Quantified_Expression";
   end Quantified_Expression;

   ---------------------
   -- Raise_Statement --
   ---------------------

   function Raise_Statement
     (Self : Node_Factory'Class; Raise_Token : Node; Raised_Exception : Node;
      With_Token      : Node; Raise_Statement_Message : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Raise_Statement unimplemented");
      return raise Program_Error with "Unimplemented function Raise_Statement";
   end Raise_Statement;

   -------------------------------
   -- Range_Attribute_Reference --
   -------------------------------

   function Range_Attribute_Reference
     (Self : Node_Factory'Class; Range_Attribute : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Range_Attribute_Reference unimplemented");
      return raise Program_Error
          with "Unimplemented function Range_Attribute_Reference";
   end Range_Attribute_Reference;

   ----------------------------------
   -- Range_Attribute_Reference_Dr --
   ----------------------------------

   function Range_Attribute_Reference_Dr
     (Self : Node_Factory'Class; Range_Attribute : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Range_Attribute_Reference_Dr unimplemented");
      return raise Program_Error
          with "Unimplemented function Range_Attribute_Reference_Dr";
   end Range_Attribute_Reference_Dr;

   -----------------------
   -- Record_Definition --
   -----------------------

   function Record_Definition
     (Self : Node_Factory'Class; Record_Token : Node; Record_Components : Node;
      End_Token : Node; End_Record_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Record_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Record_Definition";
   end Record_Definition;

   ----------------------------------
   -- Record_Representation_Clause --
   ----------------------------------

   function Record_Representation_Clause
     (Self                       : Node_Factory'Class; For_Token : Node;
      Representation_Clause_Name : Node; Use_Token : Node; Record_Token : Node;
      At_Token : Node; Mod_Token : Node; Mod_Clause_Expression : Node;
      Mod_Semicolon : Node; Component_Clauses : Node; End_Token : Node;
      End_Record                 : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Record_Representation_Clause unimplemented");
      return raise Program_Error
          with "Unimplemented function Record_Representation_Clause";
   end Record_Representation_Clause;

   ----------------------------
   -- Record_Type_Definition --
   ----------------------------

   function Record_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Tagged_Token : Node;
      Limited_Token : Node; Record_Definition : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Record_Type_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Record_Type_Definition";
   end Record_Type_Definition;

   -----------------------
   -- Requeue_Statement --
   -----------------------

   function Requeue_Statement
     (Self               : Node_Factory'Class; Requeue_Token : Node;
      Requeue_Entry_Name : Node; With_Token : Node; Abort_Token : Node;
      Semicolon_Token    : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Requeue_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function Requeue_Statement";
   end Requeue_Statement;

   ---------------------------------
   -- Return_Object_Specification --
   ---------------------------------

   function Return_Object_Specification
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Aliased_Token              : Node; Constant_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Return_Object_Specification unimplemented");
      return raise Program_Error
          with "Unimplemented function Return_Object_Specification";
   end Return_Object_Specification;

   ----------------------------------
   -- Select_Or_Else_Path_Sequence --
   ----------------------------------

   function Select_Or_Else_Path_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------
   -- Select_Or_Path --
   --------------------

   function Select_Or_Path
     (Self  : Node_Factory'Class; Or_Token : Node; When_Token : Node;
      Guard : Node; Arrow_Token : Node; Sequence_Of_Statements : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Select_Or_Path unimplemented");
      return raise Program_Error with "Unimplemented function Select_Or_Path";
   end Select_Or_Path;

   -------------------------------------
   -- Select_Then_Abort_Path_Sequence --
   -------------------------------------

   function Select_Then_Abort_Path_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ------------------------
   -- Selected_Component --
   ------------------------

   function Selected_Component
     (Self     : Node_Factory'Class; Prefix : Node; Dot_Token : Node;
      Selector : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Selected_Component unimplemented");
      return raise Program_Error
          with "Unimplemented function Selected_Component";
   end Selected_Component;

   -------------------------
   -- Selected_Identifier --
   -------------------------

   function Selected_Identifier
     (Self     : Node_Factory'Class; Prefix : Node; Dot_Token : Node;
      Selector : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Selected_Identifier unimplemented");
      return raise Program_Error
          with "Unimplemented function Selected_Identifier";
   end Selected_Identifier;

   ----------------------
   -- Selective_Accept --
   ----------------------

   function Selective_Accept
     (Self                      : Node_Factory'Class; Select_Token : Node;
      Selective_Statement_Paths : Node; End_Token : Node; End_Select : Node;
      Semicolon_Token           : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Selective_Accept unimplemented");
      return raise Program_Error
          with "Unimplemented function Selective_Accept";
   end Selective_Accept;

   -------------------
   -- Short_Circuit --
   -------------------

   function Short_Circuit
     (Self                                    : Node_Factory'Class;
      Short_Circuit_Operation_Left_Expression : Node; And_Token : Node;
      Then_Token : Node; Short_Circuit_Operation_Right_Expression : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Short_Circuit unimplemented");
      return raise Program_Error with "Unimplemented function Short_Circuit";
   end Short_Circuit;

   ------------------------------------
   -- Signed_Integer_Type_Definition --
   ------------------------------------

   function Signed_Integer_Type_Definition
     (Self : Node_Factory'Class;
      Range_Token : Node;
      Lower_Bound : Node;
      Double_Dot_Token : Node;
      Upper_Bound : Node)
      return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Signed_Integer_Type
             (Range_Token      => Range_Token.Token,
              Lower_Bound => Program.Elements.Expressions.Expression_Access
                               (Lower_Bound.Element),
              Double_Dot_Token => Double_Dot_Token.Token,
              Upper_Bound => Program.Elements.Expressions.Expression_Access
                               (Upper_Bound.Element))));
   end Signed_Integer_Type_Definition;

   -----------------------------
   -- Simple_Expression_Range --
   -----------------------------

   function Simple_Expression_Range
     (Self : Node_Factory'Class; Lower_Bound : Node; Double_Dot_Token : Node;
      Upper_Bound : Node) return Node
   is
   begin
      return
        (Element_Node,
         Program.Elements.Element_Access
           (Self.EF.Create_Simple_Expression_Range
             (Lower_Bound => Program.Elements.Expressions.Expression_Access
                               (Lower_Bound.Element),
              Double_Dot_Token => Double_Dot_Token.Token,
              Upper_Bound => Program.Elements.Expressions.Expression_Access
                               (Upper_Bound.Element))));
   end Simple_Expression_Range;

   --------------------------------
   -- Simple_Expression_Range_Dr --
   --------------------------------

   function Simple_Expression_Range_Dr
     (Self : Node_Factory'Class; Lower_Bound : Node; Double_Dot_Token : Node;
      Upper_Bound : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Simple_Expression_Range_Dr unimplemented");
      return raise Program_Error
          with "Unimplemented function Simple_Expression_Range_Dr";
   end Simple_Expression_Range_Dr;

   -----------------------------
   -- Simple_Return_Statement --
   -----------------------------

   function Simple_Return_Statement
     (Self : Node_Factory'Class; Return_Token : Node; Return_Expression : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Simple_Return_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function Simple_Return_Statement";
   end Simple_Return_Statement;

   ----------------------------------
   -- Single_Protected_Declaration --
   ----------------------------------

   function Single_Protected_Declaration
     (Self : Node_Factory'Class; Protected_Token : Node; Names : Node;
      Aspect_Specifications      : Node; Is_Token : Node; New_Token : Node;
      Progenitor_List            : Node; With_Token : Node;
      Object_Declaration_Subtype : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Single_Protected_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Single_Protected_Declaration";
   end Single_Protected_Declaration;

   -----------------------------
   -- Single_Task_Declaration --
   -----------------------------

   function Single_Task_Declaration
     (Self : Node_Factory'Class; Task_Token : Node; Names : Node;
      Aspect_Specifications      : Node; Is_Token : Node; New_Token : Node;
      Progenitor_List            : Node; With_Token : Node;
      Object_Declaration_Subtype : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Single_Task_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Single_Task_Declaration";
   end Single_Task_Declaration;

   ------------------------
   -- Statement_Sequence --
   ------------------------

   function Statement_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   -------------------------
   -- Subtype_Declaration --
   -------------------------

   function Subtype_Declaration
     (Self : Node_Factory'Class; Subtype_Token : Node; Names : Node;
      Is_Token              : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Subtype_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Subtype_Declaration";
   end Subtype_Declaration;

   ---------------------------
   -- Subtype_Mark_Sequence --
   ---------------------------

   function Subtype_Mark_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   -------------
   -- Subunit --
   -------------

   function Subunit
     (Self             : Node_Factory'Class; Context_Clause_Elements : Node;
      Separate_Token   : Node; Left_Parenthesis_Token : Node;
      Parent_Unit_Name : Node; Right_Parenthesis_Token : Node;
      Unit_Declaration : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Subunit unimplemented");
      return raise Program_Error with "Unimplemented function Subunit";
   end Subunit;

   ---------------
   -- Task_Body --
   ---------------

   function Task_Body
     (Self : Node_Factory'Class; Task_Token : Node; Body_Token : Node;
      Names : Node; Aspect_Specifications : Node; Is_Token : Node;
      Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements        : Node; Exception_Token : Node;
      Exception_Handlers     : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token        : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Task_Body unimplemented");
      return raise Program_Error with "Unimplemented function Task_Body";
   end Task_Body;

   --------------------
   -- Task_Body_Stub --
   --------------------

   function Task_Body_Stub
     (Self : Node_Factory'Class; Task_Token : Node; Body_Token : Node;
      Names                 : Node; Is_Token : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Task_Body_Stub unimplemented");
      return raise Program_Error with "Unimplemented function Task_Body_Stub";
   end Task_Body_Stub;

   ---------------------
   -- Task_Definition --
   ---------------------

   function Task_Definition
     (Self             : Node_Factory'Class; Visible_Task_Items : Node;
      Private_Token    : Node; Private_Task_Items : Node; End_Token : Node;
      Identifier_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Task_Definition unimplemented");
      return raise Program_Error with "Unimplemented function Task_Definition";
   end Task_Definition;

   ------------------------
   -- Task_Item_Sequence --
   ------------------------

   function Task_Item_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   ---------------------------
   -- Task_Type_Declaration --
   ---------------------------

   function Task_Type_Declaration
     (Self       : Node_Factory'Class; Task_Token : Node; Type_Token : Node;
      Names : Node; Discriminant_Part : Node; Aspect_Specifications : Node;
      Is_Token   : Node; New_Token : Node; Progenitor_List : Node;
      With_Token : Node; Type_Declaration_View : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Task_Type_Declaration unimplemented");
      return raise Program_Error
          with "Unimplemented function Task_Type_Declaration";
   end Task_Type_Declaration;

   -------------------------------------
   -- Terminate_Alternative_Statement --
   -------------------------------------

   function Terminate_Alternative_Statement
     (Self            : Node_Factory'Class; Terminate_Token : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Terminate_Alternative_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function Terminate_Alternative_Statement";
   end Terminate_Alternative_Statement;

   ---------------------
   -- Then_Abort_Path --
   ---------------------

   function Then_Abort_Path
     (Self : Node_Factory'Class; Then_Token : Node; Abort_Token : Node;
      Sequence_Of_Statements : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Then_Abort_Path unimplemented");
      return raise Program_Error with "Unimplemented function Then_Abort_Path";
   end Then_Abort_Path;

   --------------------------------
   -- To_Aggregate_Or_Expression --
   --------------------------------

   function To_Aggregate_Or_Expression
     (Self : Node_Factory'Class; Association_List : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "To_Aggregate_Or_Expression unimplemented");
      return raise Program_Error
          with "Unimplemented function To_Aggregate_Or_Expression";
   end To_Aggregate_Or_Expression;

   -----------------------------------
   -- To_Defining_Program_Unit_Name --
   -----------------------------------

   function To_Defining_Program_Unit_Name
     (Self : Node_Factory'Class; Selected_Identifier : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "To_Defining_Program_Unit_Name unimplemented");
      return raise Program_Error
          with "Unimplemented function To_Defining_Program_Unit_Name";
   end To_Defining_Program_Unit_Name;

   ---------------------------
   -- To_Subtype_Indication --
   ---------------------------

   function To_Subtype_Indication
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Mark : Node; Constraint : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "To_Subtype_Indication unimplemented");
      return raise Program_Error
          with "Unimplemented function To_Subtype_Indication";
   end To_Subtype_Indication;

   -----------
   -- Token --
   -----------

   function Token
     (Self  : Node_Factory'Class;
      Value : not null Program.Lexical_Elements.Lexical_Element_Access)
        return Node is
   begin
      return (Token_Node, Value);
   end Token;

   ------------------------------------
   -- Unconstrained_Array_Definition --
   ------------------------------------

   function Unconstrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Index_Subtype_Definitions  : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Unconstrained_Array_Definition unimplemented");
      return raise Program_Error
          with "Unimplemented function Unconstrained_Array_Definition";
   end Unconstrained_Array_Definition;

   -------------------------------
   -- Unknown_Discriminant_Part --
   -------------------------------

   function Unknown_Discriminant_Part
     (Self        : Node_Factory'Class; Left_Token : Node; Box_Token : Node;
      Right_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Unknown_Discriminant_Part unimplemented");
      return raise Program_Error
          with "Unimplemented function Unknown_Discriminant_Part";
   end Unknown_Discriminant_Part;

   ------------------------
   -- Use_Package_Clause --
   ------------------------

   function Use_Package_Clause
     (Self : Node_Factory'Class; Use_Token : Node; Clause_Names : Node;
      Semicolon_Token : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Use_Package_Clause unimplemented");
      return raise Program_Error
          with "Unimplemented function Use_Package_Clause";
   end Use_Package_Clause;

   ---------------------
   -- Use_Type_Clause --
   ---------------------

   function Use_Type_Clause
     (Self       : Node_Factory'Class; Use_Token : Node; All_Token : Node;
      Type_Token : Node; Type_Clause_Names : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Use_Type_Clause unimplemented");
      return raise Program_Error with "Unimplemented function Use_Type_Clause";
   end Use_Type_Clause;

   -------------
   -- Variant --
   -------------

   function Variant
     (Self : Node_Factory'Class; When_Token : Node; Variant_Choices : Node;
      Arrow_Token : Node; Record_Components : Node) return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Variant unimplemented");
      return raise Program_Error with "Unimplemented function Variant";
   end Variant;

   ------------------
   -- Variant_Part --
   ------------------

   function Variant_Part
     (Self                     : Node_Factory'Class; Case_Token : Node;
      Discriminant_Direct_Name : Node; Is_Token : Node; Variants : Node;
      End_Token : Node; End_Case_Token : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Variant_Part unimplemented");
      return raise Program_Error with "Unimplemented function Variant_Part";
   end Variant_Part;

   ----------------------
   -- Variant_Sequence --
   ----------------------

   function Variant_Sequence
     (Self : Node_Factory'Class) return Node renames New_Element_Sequence;

   --------------------------
   -- While_Loop_Statement --
   --------------------------

   function While_Loop_Statement
     (Self        : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token : Node; While_Token : Node; While_Condition : Node;
      Loop_Token  : Node; Loop_Statements : Node; End_Token : Node;
      End_Loop    : Node; Identifier_Token : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "While_Loop_Statement unimplemented");
      return raise Program_Error
          with "Unimplemented function While_Loop_Statement";
   end While_Loop_Statement;

   -----------------
   -- With_Clause --
   -----------------

   function With_Clause
     (Self : Node_Factory'Class; Limited_Token : Node; Private_Token : Node;
      With_Token : Node; With_Clause_Names : Node; Semicolon_Token : Node)
      return Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "With_Clause unimplemented");
      return raise Program_Error with "Unimplemented function With_Clause";
   end With_Clause;

end Program.Parsers.Nodes;
