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

with Asis.Elements;
with Asis.Compilation_Units;

with Gela.Compilations;
with Gela.Element_Visiters;
with Gela.Elements.Basic_Declarative_Items;
with Gela.Elements.Component_Declarations;
with Gela.Elements.Component_Definitions;
with Gela.Elements.Declarative_Items;
with Gela.Elements.Defining_Enumeration_Literals;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Operator_Symbols;
with Gela.Elements.Defining_Program_Unit_Names;
with Gela.Elements.Discriminant_Specifications;
with Gela.Elements.Entry_Bodies;
with Gela.Elements.Formal_Object_Declarations;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Function_Bodies;
with Gela.Elements.Function_Declarations;
with Gela.Elements.Object_Declarations;
with Gela.Elements.Object_Definitions;
with Gela.Elements.Object_Renaming_Declarations;
with Gela.Elements.Package_Bodies;
with Gela.Elements.Package_Declarations;
with Gela.Elements.Parameter_Specifications;
with Gela.Elements.Procedure_Bodies;
with Gela.Elements.Procedure_Declarations;
with Gela.Elements.Protected_Definitions;
with Gela.Elements.Single_Protected_Declarations;
with Gela.Elements.Single_Task_Declarations;
with Gela.Elements.Statements;
with Gela.Elements.Subtype_Mark_Or_Access_Definitions;
with Gela.Elements.Task_Bodies;
with Gela.Elements.Task_Definitions;
with Gela.Elements.Type_Definitions;
with Gela.Lexical_Types;

package body Asis.Declarations is

   --------------------------
   -- Body_Block_Statement --
   --------------------------

   function Body_Block_Statement
     (Declaration : in Asis.Declaration)
      return Asis.Statement
   is
   begin
      Check_Nil_Element (Declaration, "Body_Block_Statement");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Body_Block_Statement;

   ----------------------------
   -- Body_Declarative_Items --
   ----------------------------

   function Body_Declarative_Items
     (Declaration     : in Asis.Declaration;
      Include_Pragmas : in Boolean := False)
      return Asis.Element_List
   is
      pragma Unreferenced (Include_Pragmas);

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Declarative_Items.
              Declarative_Item_Sequence_Access;
         end record;

         overriding procedure Entry_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access);

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access);

         overriding procedure Package_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Bodies.Package_Body_Access);

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access);

         overriding procedure Task_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Bodies.Task_Body_Access);

      end Get;

      package body Get is

         overriding procedure Entry_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access) is
         begin
            Self.Result := Node.Body_Declarative_Items;
         end Entry_Body;

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access) is
         begin
            Self.Result := Node.Body_Declarative_Items;
         end Function_Body;

         overriding procedure Package_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Bodies.Package_Body_Access)
         is
         begin
            Self.Result := Node.Body_Declarative_Items;
         end Package_Body;

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access) is
         begin
            Self.Result := Node.Body_Declarative_Items;
         end Procedure_Body;

         overriding procedure Task_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Bodies.Task_Body_Access) is
         begin
            Self.Result := Node.Body_Declarative_Items;
         end Task_Body;

      end Get;

      V : Get.Visiter;
      Result : Gela.Elements.Element_Sequence_Access;
   begin
      Check_Nil_Element (Declaration, "Body_Declarative_Items");
      Declaration.Data.Visit (V);
      Result := Gela.Elements.Element_Sequence_Access (V.Result);

      return Asis.To_List (Result);
   end Body_Declarative_Items;

   -----------------------------
   -- Body_Exception_Handlers --
   -----------------------------

   function Body_Exception_Handlers
     (Declaration     : in Asis.Declaration;
      Include_Pragmas : in Boolean := False)
      return Asis.Exception_Handler_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Declaration, "Body_Exception_Handlers");
      Raise_Not_Implemented ("");
      return Nil_Element_List;
   end Body_Exception_Handlers;

   ---------------------
   -- Body_Statements --
   ---------------------

   function Body_Statements
     (Declaration     : in Asis.Declaration;
      Include_Pragmas : in Boolean := False)
      return Asis.Statement_List
   is
      pragma Unreferenced (Include_Pragmas);

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Statements.Statement_Sequence_Access;
         end record;

         overriding procedure Entry_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access);

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access);

         overriding procedure Package_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Bodies.Package_Body_Access);

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access);

         overriding procedure Task_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Bodies.Task_Body_Access);

      end Get;

      package body Get is

         overriding procedure Entry_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access) is
         begin
            Self.Result := Node.Body_Statements;
         end Entry_Body;

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access) is
         begin
            Self.Result := Node.Body_Statements;
         end Function_Body;

         overriding procedure Package_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Bodies.Package_Body_Access)
         is
         begin
            Self.Result := Node.Body_Statements;
         end Package_Body;

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access) is
         begin
            Self.Result := Node.Body_Statements;
         end Procedure_Body;

         overriding procedure Task_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Task_Bodies.Task_Body_Access) is
         begin
            Self.Result := Node.Body_Statements;
         end Task_Body;

      end Get;

      V : Get.Visiter;
      Result : Gela.Elements.Element_Sequence_Access;
   begin
      Check_Nil_Element (Declaration, "Body_Statements");
      Declaration.Data.Visit (V);
      Result := Gela.Elements.Element_Sequence_Access (V.Result);

      return Asis.To_List (Result);
   end Body_Statements;

   -------------------------------
   -- Corresponding_Base_Entity --
   -------------------------------

   function Corresponding_Base_Entity
     (Declaration : in Asis.Declaration)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_Base_Entity");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Base_Entity;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      case Asis.Elements.Declaration_Kind (Declaration) is
         when
           A_Function_Body_Declaration
           | A_Function_Body_Stub
           | A_Function_Renaming_Declaration
           | A_Package_Body_Declaration
           | A_Package_Body_Stub
           | A_Package_Renaming_Declaration
           | A_Procedure_Body_Declaration
           | A_Procedure_Renaming_Declaration
           | A_Procedure_Body_Stub
           | A_Task_Body_Declaration
           | A_Task_Body_Stub
           | A_Protected_Body_Declaration
           | A_Protected_Body_Stub
           | A_Generic_Package_Renaming_Declaration
           | A_Generic_Procedure_Renaming_Declaration
           | A_Generic_Function_Renaming_Declaration
           | An_Entry_Body_Declaration =>
            return Declaration;
         when others =>
            null;
      end case;

      Check_Nil_Element (Declaration, "Corresponding_Body");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body
     (Declaration : in Asis.Declaration;
      The_Context : in Asis.Context)
      return Asis.Declaration
   is
      pragma Unreferenced (The_Context);
   begin
      Check_Nil_Element (Declaration, "Corresponding_Body");
--      Check_Context (The_Context);
      Raise_Not_Implemented ("");
      return Corresponding_Body (Declaration);
   end Corresponding_Body;

   -----------------------------
   -- Corresponding_Body_Stub --
   -----------------------------

   function Corresponding_Body_Stub
     (Subunit : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Subunit, "Corresponding_Body_Stub");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Body_Stub;

   -----------------------------
   -- Corresponding_Body_Stub --
   -----------------------------

   function Corresponding_Body_Stub
     (Subunit     : in Asis.Declaration;
      The_Context : in Asis.Context)
      return Asis.Declaration
   is
      pragma Unreferenced (The_Context);
   begin
      Check_Nil_Element (Subunit, "Corresponding_Body_Stub");
--      Check_Context (The_Context);
      Raise_Not_Implemented ("");
      return Corresponding_Body_Stub (Subunit);
   end Corresponding_Body_Stub;

   ----------------------------------------
   -- Corresponding_Constant_Declaration --
   ----------------------------------------

   function Corresponding_Constant_Declaration
     (Name : in Asis.Defining_Name)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Name, "Corresponding_Constant_Declaration");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Constant_Declaration;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      case Asis.Elements.Declaration_Kind (Declaration) is
         when
           A_Function_Declaration
           | A_Generic_Function_Declaration
           | A_Generic_Package_Declaration
           | A_Generic_Procedure_Declaration
           --  | A_Package_Declaration  return Limited_View or its completion
           | A_Package_Renaming_Declaration
           | A_Procedure_Declaration
           | A_Single_Task_Declaration
           | A_Task_Type_Declaration
           | A_Protected_Type_Declaration
           | A_Single_Protected_Declaration
           | A_Generic_Package_Renaming_Declaration
           | A_Generic_Procedure_Renaming_Declaration
           | A_Generic_Function_Renaming_Declaration
           | An_Entry_Declaration =>
            return Declaration;
         when others =>
            null;
      end case;

      Check_Nil_Element (Declaration, "Corresponding_Declaration");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Declaration;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Declaration : in Asis.Declaration;
      The_Context : in Asis.Context)
      return Asis.Declaration
   is
      pragma Unreferenced (The_Context);
   begin
      Check_Nil_Element (Declaration, "Corresponding_Declaration");
--      Check_Context (The_Context);
      Raise_Not_Implemented ("");
      return Corresponding_Declaration (Declaration);
   end Corresponding_Declaration;

   -------------------------------------
   -- Corresponding_Equality_Operator --
   -------------------------------------

   function Corresponding_Equality_Operator
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_Equality_Operator");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Equality_Operator;

   ---------------------------------
   -- Corresponding_First_Subtype --
   ---------------------------------

   function Corresponding_First_Subtype
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_First_Subtype");

      case Asis.Elements.Declaration_Kind (Declaration) is
         when An_Ordinary_Type_Declaration |
           A_Task_Type_Declaration |
           A_Protected_Type_Declaration |
           A_Private_Type_Declaration |
           A_Private_Extension_Declaration |
           A_Formal_Type_Declaration =>
            return Declaration;
         when others =>
            Raise_Not_Implemented ("");
            return Nil_Element;
      end case;
   end Corresponding_First_Subtype;

   -----------------------------------
   -- Corresponding_Generic_Element --
   -----------------------------------

   function Corresponding_Generic_Element
     (Reference : in Asis.Element)
      return Asis.Defining_Name
   is
   begin
      Check_Nil_Element (Reference, "Corresponding_Generic_Element");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Generic_Element;

   -----------------------------------
   -- Corresponding_Last_Constraint --
   -----------------------------------

   function Corresponding_Last_Constraint
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_Last_Constraint");

      case Asis.Elements.Declaration_Kind (Declaration) is
         when An_Ordinary_Type_Declaration |
           A_Task_Type_Declaration |
           A_Protected_Type_Declaration |
           A_Private_Type_Declaration |
           A_Private_Extension_Declaration |
           A_Formal_Type_Declaration =>
            return Declaration;
         when others =>
            Raise_Not_Implemented ("");
            return Nil_Element;
      end case;
   end Corresponding_Last_Constraint;

   --------------------------------
   -- Corresponding_Last_Subtype --
   --------------------------------

   function Corresponding_Last_Subtype
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_Last_Subtype");

      case Asis.Elements.Declaration_Kind (Declaration) is
         when An_Ordinary_Type_Declaration |
           A_Task_Type_Declaration |
           A_Protected_Type_Declaration |
           A_Private_Type_Declaration |
           A_Private_Extension_Declaration |
           A_Formal_Type_Declaration =>
            return Declaration;
         when others =>
            Raise_Not_Implemented ("");
            return Nil_Element;
      end case;
   end Corresponding_Last_Subtype;

   ------------------------------------------
   -- Corresponding_Representation_Clauses --
   ------------------------------------------

   function Corresponding_Representation_Clauses
     (Declaration : in Asis.Declaration)
      return Asis.Representation_Clause_List
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_Representation_Clauses");
      Raise_Not_Implemented ("");
      return Nil_Element_List;
   end Corresponding_Representation_Clauses;

   -----------------------------------------
   -- Corresponding_Subprogram_Derivation --
   -----------------------------------------

   function Corresponding_Subprogram_Derivation
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_Subprogram_Derivation");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Subprogram_Derivation;

   ---------------------------
   -- Corresponding_Subunit --
   ---------------------------

   function Corresponding_Subunit
     (Body_Stub : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Body_Stub, "Corresponding_Subunit");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Subunit;

   ---------------------------
   -- Corresponding_Subunit --
   ---------------------------

   function Corresponding_Subunit
     (Body_Stub   : in Asis.Declaration;
      The_Context : in Asis.Context)
      return Asis.Declaration
   is
      pragma Unreferenced (The_Context);
   begin
      Check_Nil_Element (Body_Stub, "Corresponding_Subunit");
--      Check_Context (The_Context);
      Raise_Not_Implemented ("");
      return Corresponding_Subunit (Body_Stub);
   end Corresponding_Subunit;

   ------------------------
   -- Corresponding_Type --
   ------------------------

   function Corresponding_Type
     (Declaration : in Asis.Declaration)
      return Asis.Type_Definition
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access);

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access);
      end Get;

      package body Get is

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access) is
         begin
            Self.Result := Node.Corresponding_Type;
         end Function_Declaration;

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access) is
         begin
            Self.Result := Node.Corresponding_Type;
         end Procedure_Declaration;
      end Get;

      V       : Get.Visiter;
   begin
      Check_Nil_Element (Declaration, "Corresponding_Type");
      Declaration.Data.Visit (V);
      return Asis.Declarations.Type_Declaration_View ((Data => V.Result));
   end Corresponding_Type;

   ------------------------------------
   -- Corresponding_Type_Declaration --
   ------------------------------------

   function Corresponding_Type_Declaration
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Declaration, "Corresponding_Type_Declaration");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Corresponding_Type_Declaration;

   ------------------------------------
   -- Corresponding_Type_Declaration --
   ------------------------------------

   function Corresponding_Type_Declaration
     (Declaration : in Asis.Declaration;
      The_Context : in Asis.Context)
      return Asis.Declaration
   is
      pragma Unreferenced (The_Context);
   begin
      Check_Nil_Element (Declaration, "Corresponding_Type_Declaration");
--      Check_Context (The_Context);
      return Corresponding_Type_Declaration (Declaration);
   end Corresponding_Type_Declaration;

   ------------------------------
   -- Declaration_Subtype_Mark --
   ------------------------------

   function Declaration_Subtype_Mark
     (Declaration : in Asis.Declaration)
      return Asis.Expression
   is
      Result : Asis.Definition;
   begin
      Check_Nil_Element (Declaration, "Declaration_Subtype_Mark");

      case Asis.Elements.Declaration_Kind (Declaration) is
         when A_Variable_Declaration |
           A_Constant_Declaration |
           A_Deferred_Constant_Declaration |
           A_Single_Protected_Declaration |
           A_Single_Task_Declaration |
           A_Component_Declaration
           =>
--            Raise_Inappropriate_Element ("Declaration_Subtype_Mark");
            Raise_Not_Implemented ("");
            return Nil_Element;
         when others =>
            null;
      end case;

      Result := Object_Declaration_Subtype (Declaration);

      if Assigned (Result) then
         case Asis.Elements.Definition_Kind (Result) is
            when A_Subtype_Indication =>
               Raise_Not_Implemented ("");
               return Nil_Element;
--               return Get_Subtype_Mark (Result.all);
            when An_Access_Definition =>
               case Asis.Elements.Access_Definition_Kind (Result) is
                  when An_Anonymous_Access_To_Object_Definition =>
                     Raise_Not_Implemented ("");
                     return Nil_Element;
--                     return
--                       Anonymous_Access_To_Object_Subtype_Mark (Result.all);
                  when others =>
                     Raise_Not_Implemented ("");
                     return Nil_Element;
               end case;
            when others =>
               Raise_Not_Implemented ("");
               return Nil_Element;
         end case;
      end if;

      return Result;
   end Declaration_Subtype_Mark;

   -------------------------
   -- Defining_Name_Image --
   -------------------------

   function Defining_Name_Image
     (Defining_Name : in Asis.Defining_Name)
      return Program_Text
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Symbol : Gela.Lexical_Types.Symbol := 0;
         end record;

         overriding procedure Defining_Enumeration_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Enumeration_Literals.
              Defining_Enumeration_Literal_Access);

         overriding procedure Defining_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access);

         overriding procedure Defining_Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Operator_Symbols.
              Defining_Operator_Symbol_Access);
      end Get;

      package body Get is

         overriding procedure Defining_Enumeration_Literal
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Enumeration_Literals.
              Defining_Enumeration_Literal_Access)
         is
            Token : constant Gela.Lexical_Types.Token_Count :=
              Node.Identifier;
            Comp  : constant Gela.Compilations.Compilation_Access :=
              Node.Enclosing_Compilation;
         begin
            Self.Symbol := Comp.Get_Token (Token).Symbol;
         end Defining_Enumeration_Literal;

         overriding procedure Defining_Identifier
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access)
         is
            Token : constant Gela.Lexical_Types.Token_Count :=
              Node.Identifier_Token;
            Comp  : constant Gela.Compilations.Compilation_Access :=
              Node.Enclosing_Compilation;
         begin
            Self.Symbol := Comp.Get_Token (Token).Symbol;
         end Defining_Identifier;

         overriding procedure Defining_Operator_Symbol
           (Self : in out Visiter;
            Node : not null Gela.Elements.Defining_Operator_Symbols.
              Defining_Operator_Symbol_Access)
         is
            Token : constant Gela.Lexical_Types.Token_Count :=
              Node.Operator_Symbol_Token;
            Comp  : constant Gela.Compilations.Compilation_Access :=
              Node.Enclosing_Compilation;
         begin
            Self.Symbol := Comp.Get_Token (Token).Symbol;
         end Defining_Operator_Symbol;
      end Get;

      V       : Get.Visiter;
      Comp    : Gela.Compilations.Compilation_Access;
      Context : Gela.Contexts.Context_Access;
   begin
      Check_Nil_Element (Defining_Name, "Defining_Name_Image");
      Defining_Name.Data.Visit (V);
      Comp := Defining_Name.Data.Enclosing_Compilation;
      Context := Comp.Context;
      return Context.Symbols.Image (V.Symbol).To_UTF_16_Wide_String;
   end Defining_Name_Image;

   ---------------------
   -- Defining_Prefix --
   ---------------------

   function Defining_Prefix
     (Defining_Name : in Asis.Defining_Name)
      return Asis.Name
   is
   begin
      Check_Nil_Element (Defining_Name, "Defining_Prefix");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Defining_Prefix;

   -----------------------
   -- Defining_Selector --
   -----------------------

   function Defining_Selector
     (Defining_Name : in Asis.Defining_Name)
      return Asis.Defining_Name
   is
   begin
      Check_Nil_Element (Defining_Name, "Defining_Selector");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Defining_Selector;

   -----------------------
   -- Discriminant_Part --
   -----------------------

   function Discriminant_Part
     (Declaration : in Asis.Declaration)
      return Asis.Definition
   is
   begin
      Check_Nil_Element (Declaration, "Discriminant_Part");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Discriminant_Part;

   -------------------
   -- Entry_Barrier --
   -------------------

   function Entry_Barrier
     (Declaration : in Asis.Declaration)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Declaration, "Entry_Barrier");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Entry_Barrier;

   -----------------------------
   -- Entry_Family_Definition --
   -----------------------------

   function Entry_Family_Definition
     (Declaration : in Asis.Declaration)
      return Asis.Discrete_Subtype_Definition
   is
   begin
      Check_Nil_Element (Declaration, "Entry_Family_Definition");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Entry_Family_Definition;

   -------------------------------
   -- Entry_Index_Specification --
   -------------------------------

   function Entry_Index_Specification
     (Declaration : in Asis.Declaration)
      return Asis.Declaration
   is
   begin
      Check_Nil_Element (Declaration, "Entry_Index_Specification");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Entry_Index_Specification;

   -------------------------------
   -- Formal_Subprogram_Default --
   -------------------------------

   function Formal_Subprogram_Default
     (Declaration : in Asis.Generic_Formal_Parameter)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Declaration, "Formal_Subprogram_Default");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Formal_Subprogram_Default;

   -------------------------
   -- Generic_Actual_Part --
   -------------------------

   function Generic_Actual_Part
     (Declaration : in Asis.Declaration;
      Normalized  : in Boolean := False)
      return Asis.Association_List
   is
   begin
      Check_Nil_Element (Declaration, "Generic_Actual_Part");
      if Normalized then
         Raise_Not_Implemented ("");
         return Nil_Element_List;
      else
         Raise_Not_Implemented ("");
         return Nil_Element_List;
      end if;
   end Generic_Actual_Part;

   -------------------------
   -- Generic_Formal_Part --
   -------------------------

   function Generic_Formal_Part
     (Declaration     : in Asis.Declaration;
      Include_Pragmas : in Boolean := False)
      return Asis.Element_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Declaration, "Generic_Formal_Part");
      Raise_Not_Implemented ("");
      return Nil_Element_List;
   end Generic_Formal_Part;

   -----------------------
   -- Generic_Unit_Name --
   -----------------------

   function Generic_Unit_Name
     (Declaration : in Asis.Declaration)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Declaration, "Generic_Unit_Name");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Generic_Unit_Name;

   -------------------------------
   -- Initialization_Expression --
   -------------------------------

   function Initialization_Expression
     (Declaration : in Asis.Declaration)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Declaration, "Initialization_Expression");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Initialization_Expression;

   ------------------------------
   -- Is_Dispatching_Operation --
   ------------------------------

   function Is_Dispatching_Operation
     (Declaration : in Asis.Element)
      return Boolean
   is
   begin
      Check_Nil_Element (Declaration, "Is_Dispatching_Operation");
      Raise_Not_Implemented ("");
      return False;
   end Is_Dispatching_Operation;

   ----------------------
   -- Is_Name_Repeated --
   ----------------------

   function Is_Name_Repeated
     (Declaration : in Asis.Declaration)
      return Boolean
   is
   begin
      Check_Nil_Element (Declaration, "Is_Name_Repeated");
      Raise_Not_Implemented ("");
      return False;
   end Is_Name_Repeated;

   ------------------------
   -- Is_Private_Present --
   ------------------------

   function Is_Private_Present
     (Declaration : in Asis.Declaration)
      return Boolean
   is
   begin
      Check_Nil_Element (Declaration, "Is_Private_Present");
      Raise_Not_Implemented ("");
      return False;
   end Is_Private_Present;

   ----------------
   -- Is_Subunit --
   ----------------

   function Is_Subunit (Declaration : in Asis.Declaration) return Boolean is
      Enclosing_Unit : constant Compilation_Unit :=
         Asis.Elements.Enclosing_Compilation_Unit (Declaration);
   begin
      if Assigned (Declaration) then
         return Asis.Elements.Is_Equal
                  (Declaration,
                   Asis.Elements.Unit_Declaration (Enclosing_Unit))
           and Asis.Compilation_Units.Unit_Kind (Enclosing_Unit) in A_Subunit;
      else
         return False;
      end if;
   end Is_Subunit;

   -----------
   -- Names --
   -----------

   function Names
     (Declaration : in Asis.Declaration)
      return Asis.Defining_Name_List
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Name  : Gela.Elements.Element_Access;
            Names : Gela.Elements.Element_Sequence_Access;
         end record;

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access);

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access);

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access);

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access);

      end Get;

      package body Get is

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access)
         is
            Name : constant Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access := Node.Names;
         begin
            Self.Name := Gela.Elements.Element_Access (Name);
         end Full_Type_Declaration;

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access)
         is
            Names : constant Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Sequence_Access := Node.Names;
         begin
            Self.Names := Gela.Elements.Element_Sequence_Access (Names);
         end Object_Declaration;

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access)
         is
            Name : constant Gela.Elements.Defining_Program_Unit_Names.
              Defining_Program_Unit_Name_Access := Node.Names;
         begin
            Self.Name := Gela.Elements.Element_Access (Name);
         end Procedure_Body;

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access)
         is
            Name : constant Gela.Elements.Defining_Program_Unit_Names.
              Defining_Program_Unit_Name_Access := Node.Names;
         begin
            Self.Name := Gela.Elements.Element_Access (Name);
         end Procedure_Declaration;

      end Get;

      use type Gela.Elements.Element_Access;
      use type Gela.Elements.Element_Sequence_Access;
      V : Get.Visiter;
   begin
      Check_Nil_Element (Declaration, "Names");
      Declaration.Data.Visit (V);

      if V.Name /= null then
         return (1 => (Data => V.Name));
      elsif V.Names /= null then
         return Asis.To_List (V.Names);
      else
         Raise_Not_Implemented ("");
         return Asis.Nil_Element_List;
      end if;
   end Names;

   -----------------------------
   -- Object_Declaration_View --
   -----------------------------

   function Object_Declaration_View
     (Declaration : in Asis.Declaration)
      return Asis.Definition
   is
      Result : Asis.Definition;
   begin
      Check_Nil_Element (Declaration, "Object_Declaration_View");

      case Asis.Elements.Declaration_Kind (Declaration) is
         when A_Discriminant_Specification |
           A_Parameter_Specification |
           A_Formal_Object_Declaration |
           An_Object_Renaming_Declaration
           =>
--            Raise_Inappropriate_Element ("Object_Declaration_View");
            Raise_Not_Implemented ("");
            return Nil_Element;
         when others =>
            null;
      end case;

      Result := Object_Declaration_Subtype (Declaration);

      if Assigned (Result) and then
        Asis.Elements.Definition_Kind (Result) = An_Access_Definition
      then
         Raise_Not_Implemented ("");
         return Nil_Element;
      end if;

      return Result;
   end Object_Declaration_View;

   --------------------------------
   -- Object_Declaration_Subtype --
   --------------------------------

   function Object_Declaration_Subtype
     (Declaration : in Asis.Declaration) return Asis.Definition
   is

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access);

         overriding procedure Discriminant_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Access);

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access);

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access);

         overriding procedure Object_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Renaming_Declarations.
              Object_Renaming_Declaration_Access);

         overriding procedure Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Access);

         overriding procedure Single_Protected_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Protected_Declarations.
              Single_Protected_Declaration_Access);

         overriding procedure Single_Task_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Task_Declarations.
              Single_Task_Declaration_Access);
      end Get;

      package body Get is

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access)
         is
            X : constant Gela.Elements.Component_Definitions.
              Component_Definition_Access := Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Component_Declaration;

         overriding procedure Discriminant_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Access)
         is
            X : constant Gela.Elements.Subtype_Mark_Or_Access_Definitions.
              Subtype_Mark_Or_Access_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Discriminant_Specification;

         overriding procedure Formal_Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Formal_Object_Declarations.
              Formal_Object_Declaration_Access)
         is
            X : constant Gela.Elements.Subtype_Mark_Or_Access_Definitions.
              Subtype_Mark_Or_Access_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Formal_Object_Declaration;

         overriding procedure Object_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Declarations.
              Object_Declaration_Access)
         is
            X : constant Gela.Elements.Object_Definitions.
              Object_Definition_Access := Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Object_Declaration;

         overriding procedure Object_Renaming_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Object_Renaming_Declarations.
              Object_Renaming_Declaration_Access)
         is
            X : constant Gela.Elements.Subtype_Mark_Or_Access_Definitions.
              Subtype_Mark_Or_Access_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Object_Renaming_Declaration;

         overriding procedure Parameter_Specification
           (Self : in out Visiter;
            Node : not null Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Access)
         is
            X : constant Gela.Elements.Subtype_Mark_Or_Access_Definitions.
              Subtype_Mark_Or_Access_Definition_Access :=
                Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Parameter_Specification;

         overriding procedure Single_Protected_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Protected_Declarations.
              Single_Protected_Declaration_Access)
         is
            X : constant Gela.Elements.Protected_Definitions.
              Protected_Definition_Access := Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Single_Protected_Declaration;

         overriding procedure Single_Task_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Single_Task_Declarations.
              Single_Task_Declaration_Access)
         is
            X : constant Gela.Elements.Task_Definitions.
              Task_Definition_Access := Node.Object_Declaration_Subtype;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Single_Task_Declaration;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Declaration, "Object_Declaration_Subtype");
      Declaration.Data.Visit (V);

      return (Data => V.Result);
   end Object_Declaration_Subtype;

   -------------------------------
   -- Overriding_Indicator_Kind --
   -------------------------------

   function Overriding_Indicator_Kind
     (Declaration : Asis.Declaration)
     return Asis.Overriding_Indicator_Kinds
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_An_Overriding_Indicator;
      else
         return Not_An_Overriding_Indicator;
      end if;
   end Overriding_Indicator_Kind;

   -----------------------
   -- Parameter_Profile --
   -----------------------

   function Parameter_Profile
     (Declaration : in Asis.Declaration)
      return Asis.Parameter_Specification_List
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            List : Gela.Elements.Element_Sequence_Access;
         end record;

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access);

      end Get;

      package body Get is

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access)
         is
            List : constant Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Sequence_Access :=
                Node.Parameter_Profile;
         begin
            Self.List := Gela.Elements.Element_Sequence_Access (List);
         end Procedure_Declaration;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Declaration, "Parameter_Profile");
      Declaration.Data.Visit (V);

      return Asis.To_List (V.List);
   end Parameter_Profile;

   ---------------------------
   -- Position_Number_Image --
   ---------------------------

   function Position_Number_Image
     (Defining_Name : in Asis.Defining_Name)
      return Wide_String
   is
   begin
      Check_Nil_Element (Defining_Name, "Position_Number_Image");
      Raise_Not_Implemented ("");
      return "";
   end Position_Number_Image;

   ------------------------------------
   -- Private_Part_Declarative_Items --
   ------------------------------------

   function Private_Part_Declarative_Items
     (Declaration     : in Asis.Declaration;
      Include_Pragmas : in Boolean := False)
      return Asis.Declarative_Item_List
   is
      pragma Unreferenced (Include_Pragmas);
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            List : Gela.Elements.Element_Sequence_Access;
         end record;

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access);

      end Get;

      package body Get is

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access)
         is
            List : constant Gela.Elements.Basic_Declarative_Items.
              Basic_Declarative_Item_Sequence_Access :=
                Node.Private_Part_Declarative_Items;
         begin
            Self.List := Gela.Elements.Element_Sequence_Access (List);
         end Package_Declaration;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Declaration, "Private_Part_Declarative_Items");
      Declaration.Data.Visit (V);

      return Asis.To_List (V.List);
   end Private_Part_Declarative_Items;

   ---------------------
   -- Progenitor_List --
   ---------------------

   function Progenitor_List
     (Declaration : Asis.Declaration)
     return Asis.Name_List is
   begin
      Check_Nil_Element (Declaration, "Progenitor_List");
      Raise_Not_Implemented ("");
      return Nil_Element_List;
   end Progenitor_List;

   -------------------------------
   -- Protected_Operation_Items --
   -------------------------------

   function Protected_Operation_Items
     (Declaration     : in Asis.Declaration;
      Include_Pragmas : in Boolean := False)
      return Asis.Declaration_List
   is
      pragma Unreferenced (Include_Pragmas);
   begin
      Check_Nil_Element (Declaration, "Protected_Operation_Items");
      Raise_Not_Implemented ("");
      return Nil_Element_List;
   end Protected_Operation_Items;

   --------------------
   -- Renamed_Entity --
   --------------------

   function Renamed_Entity
     (Declaration : in Asis.Declaration)
      return Asis.Expression
   is
   begin
      Check_Nil_Element (Declaration, "Renamed_Entity");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Renamed_Entity;

   --------------------------------
   -- Representation_Value_Image --
   --------------------------------

   function Representation_Value_Image
     (Defining_Name : in Asis.Defining_Name)
      return Wide_String
   is
   begin
      Check_Nil_Element (Defining_Name, "Representation_Value_Image");
      Raise_Not_Implemented ("");
      return "";
   end Representation_Value_Image;

   --------------------
   -- Result_Profile --
   --------------------

   function Result_Profile
     (Declaration : in Asis.Declaration)
      return Asis.Expression
   is
      Result : Asis.Definition;
   begin
      Check_Nil_Element (Declaration, "Result_Profile");
--      Result := Result_Subtype (Declaration);

      if Assigned (Result) then
         case Asis.Elements.Definition_Kind (Result) is
            when A_Subtype_Indication =>
               Raise_Not_Implemented ("");
               return Nil_Element;
--               return Asis.Definitions.Subtype_Mark (Result);
            when others =>
               Raise_Not_Implemented ("");
               return Nil_Element;
         end case;
      end if;

      return Result;
   end Result_Profile;

   --------------------
   -- Result_Subtype --
   --------------------

   function Result_Subtype
     (Declaration : in Asis.Declaration)
      return Asis.Definition is
   begin
      Check_Nil_Element (Declaration, "Result_Subtype");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Result_Subtype;

   --------------------------------------
   -- Specification_Subtype_Definition --
   --------------------------------------

   function Specification_Subtype_Definition
     (Specification : in Asis.Declaration)
      return Asis.Discrete_Subtype_Definition
   is
   begin
      Check_Nil_Element (Specification, "Specification_Subtype_Definition");
      Raise_Not_Implemented ("");
      return Nil_Element;
   end Specification_Subtype_Definition;

   ---------------------------
   -- Type_Declaration_View --
   ---------------------------

   function Type_Declaration_View
     (Declaration : in Asis.Declaration)
      return Asis.Definition
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Element_Access;
         end record;

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access);
      end Get;

      package body Get is

         overriding procedure Full_Type_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Full_Type_Declarations.
              Full_Type_Declaration_Access)
         is
            X : constant Gela.Elements.Type_Definitions.Type_Definition_Access
              := Node.Type_Declaration_View;
         begin
            Self.Result := Gela.Elements.Element_Access (X);
         end Full_Type_Declaration;
      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Declaration, "Type_Declaration_View");
      Declaration.Data.Visit (V);
      return (Data => V.Result);
   end Type_Declaration_View;

   ------------------------------------
   -- Visible_Part_Declarative_Items --
   ------------------------------------

   function Visible_Part_Declarative_Items
     (Declaration     : in Asis.Declaration;
      Include_Pragmas : in Boolean := False)
      return Asis.Declarative_Item_List
   is
      pragma Unreferenced (Include_Pragmas);

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            List : Gela.Elements.Element_Sequence_Access;
         end record;

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access);

      end Get;

      package body Get is

         overriding procedure Package_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Package_Declarations.
              Package_Declaration_Access)
         is
            List : constant Gela.Elements.Basic_Declarative_Items.
              Basic_Declarative_Item_Sequence_Access :=
                Node.Visible_Part_Declarative_Items;
         begin
            Self.List := Gela.Elements.Element_Sequence_Access (List);
         end Package_Declaration;

      end Get;

      V : Get.Visiter;
   begin
      Check_Nil_Element (Declaration, "Visible_Part_Declarative_Items");
      Declaration.Data.Visit (V);

      return Asis.To_List (V.List);
   end Visible_Part_Declarative_Items;

end Asis.Declarations;


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
