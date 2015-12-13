with League.Strings;

with Asis.Elements;
with Asis.Declarations;
with Asis.Definitions;

with Gela.A4G.Contexts;

package body Gela.A4G.Elements is

   ---------------
   -- Arguments --
   ---------------

   overriding function Arguments
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Elements.Pragma_Argument_Associations (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Arguments;

   -------------
   -- Aspects --
   -------------

   overriding function Aspects
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Aspect_Specifications (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Aspects;

   ----------------------------
   -- Body_Declarative_Items --
   ----------------------------

   overriding function Body_Declarative_Items
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Body_Declarative_Items (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Body_Declarative_Items;

   -----------------------------
   -- Body_Exception_Handlers --
   -----------------------------

   overriding function Body_Exception_Handlers
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Body_Statements (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Body_Exception_Handlers;

   ---------------------
   -- Body_Statements --
   ---------------------

   overriding function Body_Statements
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Body_Statements (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Body_Statements;

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element) return Element_Access
   is
      Result : constant Element_Access := new Element'
        (Context => Context,
         Node    => Node,
         Next    => null);

   begin
      return Result;
   end Create;

   -----------------
   -- Create_List --
   -----------------

   function Create_List
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element_List) return Element_Sequence_Access
   is
      Element : Element_Access;
      Next    : Element_Access;
      Result  : Element_Sequence_Access;
   begin
      if Node'Length > 0 then
         Element := Context.Create_Element (Node (Node'First));
      end if;

      Result := new Element_Sequence'
        (Gela.Element_Sequences.Element_Sequence with
           First => Element, Length => Node'Length);

      for J in Node'First + 1 .. Node'Last loop
         Next := Context.Create_Element (Node (J));
         Element.Next := Next;
         Element := Next;
      end loop;

      return Result;
   end Create_List;

   ----------------------
   -- Compilation_Unit --
   ----------------------

   overriding function Compilation_Unit
     (Self : aliased Element)
      return Gela.Compilation_Units.Compilation_Unit_Access
   is
      Unit : constant Asis.Compilation_Unit :=
        Asis.Elements.Enclosing_Compilation_Unit (Self.Node);
   begin
      return Gela.Compilation_Units.Compilation_Unit_Access
        (Self.Context.Create_Compilation_Unit (Unit));
   end Compilation_Unit;

   -------------------
   -- Discriminants --
   -------------------

   overriding function Discriminants
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      Part : constant Asis.Definition :=
        Asis.Declarations.Discriminant_Part (Self.Node);
      List : constant Asis.Element_List :=
        Asis.Definitions.Discriminants (Part);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Discriminants;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   overriding function Enclosing_Element
     (Self : aliased Element)
      return Gela.Elements.Element_Access
   is
      Parent : constant Asis.Element :=
        Asis.Elements.Enclosing_Element (Self.Node);
      Result : constant Element_Access := Self.Context.Create_Element (Parent);
   begin
      return Gela.Elements.Element_Access (Result);
   end Enclosing_Element;

   -------------------
   -- Entry_Barrier --
   -------------------

   overriding function Entry_Barrier
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Entry_Barrier (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Entry_Barrier;

   -----------------------------
   -- Entry_Family_Definition --
   -----------------------------

   overriding function Entry_Family_Definition
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Entry_Family_Definition (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Entry_Family_Definition;

   -------------------------------
   -- Entry_Index_Specification --
   -------------------------------

   overriding function Entry_Index_Specification
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Entry_Index_Specification (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Entry_Index_Specification;

   ----------------
   -- Expression --
   ----------------

   overriding function Expression
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Result_Expression (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Expression;

   -------------------------
   -- Generic_Formal_Part --
   -------------------------

   overriding function Generic_Formal_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Generic_Formal_Part (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Generic_Formal_Part;

   -----------------
   -- Is_Abstract --
   -----------------

   overriding function Is_Abstract (Self : aliased Element) return Boolean is
   begin
      return Asis.Elements.Has_Abstract (Self.Node);
   end Is_Abstract;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Self : aliased Element_Sequence) return Boolean is
   begin
      return Self.Length = 0;
   end Is_Empty;

   ----------------------------
   -- Is_Expression_Function --
   ----------------------------

   overriding function Is_Expression_Function
     (Self : aliased Element) return Boolean is
   begin
      return Asis.Elements.Declaration_Kind (Self.Node) in
        Asis.An_Expression_Function_Declaration;
   end Is_Expression_Function;

   -----------------
   -- Is_Function --
   -----------------

   overriding function Is_Function (Self : aliased Element) return Boolean is
      Kind : constant Asis.Declaration_Kinds :=
        Asis.Elements.Declaration_Kind (Self.Node);
   begin
      return Kind in Asis.A_Function_Declaration
        | Asis.A_Function_Body_Declaration
        | Asis.An_Expression_Function_Declaration
        | Asis.A_Function_Renaming_Declaration
        | Asis.A_Generic_Function_Renaming_Declaration
        | Asis.A_Function_Body_Stub
        | Asis.A_Generic_Function_Declaration
        | Asis.A_Function_Instantiation
        | Asis.A_Formal_Function_Declaration;
   end Is_Function;

   -----------------------
   -- Is_Null_Procedure --
   -----------------------

   overriding function Is_Null_Procedure
     (Self : aliased Element) return Boolean is
   begin
      return Asis.Elements.Declaration_Kind (Self.Node) in
        Asis.A_Null_Procedure_Declaration;
   end Is_Null_Procedure;

   ------------------
   -- Is_Procedure --
   ------------------

   overriding function Is_Procedure (Self : aliased Element) return Boolean is
      Kind : constant Asis.Declaration_Kinds :=
        Asis.Elements.Declaration_Kind (Self.Node);
   begin
      return Kind in Asis.A_Procedure_Declaration
        | Asis.A_Procedure_Body_Declaration
        | Asis.A_Null_Procedure_Declaration
        | Asis.A_Procedure_Renaming_Declaration
        | Asis.A_Generic_Procedure_Renaming_Declaration
        | Asis.A_Procedure_Body_Stub
        | Asis.A_Generic_Procedure_Declaration
        | Asis.A_Procedure_Instantiation
        | Asis.A_Formal_Procedure_Declaration;
   end Is_Procedure;

   -----------------------------
   -- Is_Renaming_Declaration --
   -----------------------------

   overriding function Is_Renaming_Declaration
     (Self : aliased Element) return Boolean is
      Kind : constant Asis.Declaration_Kinds :=
        Asis.Elements.Declaration_Kind (Self.Node);
   begin
      return Kind in Asis.A_Renaming_Declaration;
   end Is_Renaming_Declaration;

   ---------------
   -- Iterators --
   ---------------

   package Iterators is
      use Gela.Element_Sequences.Element_Sequences;

      type Forward_Iterator is new Gela.Element_Sequences
        .Element_Sequences.Iterator_Interfaces.Forward_Iterator
        and Gela.Element_Sequences
          .Defining_Name_Sequences.Iterator_Interfaces.Forward_Iterator
      with record
         First : Element_Access;
      end record;

      overriding function First
        (Self : Forward_Iterator) return Gela.Elements.Element_Access;

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Elements.Element_Access)
         return Gela.Elements.Element_Access;

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

   end Iterators;

   package body Iterators is

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Forward_Iterator) return Gela.Elements.Element_Access is
      begin
         return Gela.Elements.Element_Access (Self.First);
      end First;

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Elements.Defining_Names.Defining_Name_Access is
      begin
         return Gela.Elements.Defining_Names.Defining_Name_Access (Self.First);
      end First;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Elements.Element_Access)
         return Gela.Elements.Element_Access
      is
         pragma Unreferenced (Self);
      begin
         return Gela.Elements.Element_Access
           (Element (Position.all).Next);
      end Next;

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Elements.Defining_Names.Defining_Name_Access)
         return Gela.Elements.Defining_Names.Defining_Name_Access
      is
         pragma Unreferenced (Self);
      begin
         return Gela.Elements.Defining_Names.Defining_Name_Access
           (Element (Position.all).Next);
      end Next;

   end Iterators;

   -------------
   -- Iterate --
   -------------

   overriding function Iterate
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Element_Sequences
               .Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Iterators.Forward_Iterator'(First => Self.First);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   overriding function Iterate
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Defining_Name_Sequences
               .Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Iterators.Forward_Iterator'(First => Self.First);
   end Iterate;

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : aliased Element_Sequence) return Natural is
   begin
      return Self.Length;
   end Length;

   -----------
   -- Names --
   -----------

   overriding function Names
     (Self : aliased Element)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Names (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Defining_Name_Sequence_Access (Result);
   end Names;

   -----------------------
   -- Parameter_Prolile --
   -----------------------

   overriding function Parameter_Prolile
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Parameter_Profile (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Parameter_Prolile;

   ------------
   -- Prefix --
   ------------

   overriding function Prefix
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Defining_Prefix (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Prefix;

   ------------------
   -- Private_Part --
   ------------------

   overriding function Private_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Private_Part_Declarative_Items (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Private_Part;

   --------------------
   -- Renamed_Entity --
   --------------------

   overriding function Renamed_Entity
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Renamed_Entity (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Renamed_Entity;

   --------------------
   -- Result_Subtype --
   --------------------

   overriding function Result_Subtype
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Result_Profile (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Result_Subtype;

   ------------------------
   -- Subtype_Indication --
   ------------------------

   overriding function Subtype_Indication
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Type_Declaration_View (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Subtype_Indication;

   ------------
   -- Symbol --
   ------------

   overriding function Symbol
     (Self : aliased Element) return Gela.Symbols.Symbol_Access
   is
      Text   : League.Strings.Universal_String;
      Result : Gela.Symbols.Symbol_Access :=
        Self.Context.Create_Symbol (Text);
   begin
      case Asis.Elements.Element_Kind (Self.Node) is
         when Asis.A_Pragma =>
            Text := League.Strings.From_UTF_16_Wide_String
              (Asis.Elements.Pragma_Name_Image (Self.Node));
         when Asis.A_Defining_Name =>
            Text := League.Strings.From_UTF_16_Wide_String
              (Asis.Declarations.Defining_Name_Image (Self.Node));
         when others =>
            raise Program_Error;
      end case;

      Result := Self.Context.Create_Symbol (Text);

      return Result;
   end Symbol;

   -------------------------
   -- To_Element_Sequence --
   -------------------------

   overriding function To_Element_Sequence
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Element_Sequence_Access is
   begin
      return Self'Access;
   end To_Element_Sequence;

   ---------------------
   -- Type_Definition --
   ---------------------

   overriding function Type_Definition
     (Self : aliased Element) return Gela.Elements.Element_Access
   is
      Result : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element
          (Asis.Declarations.Type_Declaration_View (Self.Node));
   begin
      return Gela.Elements.Element_Access (Result);
   end Type_Definition;

   ------------------
   -- Visible_Part --
   ------------------

   overriding function Visible_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access
   is
      List : constant Asis.Element_List :=
        Asis.Declarations.Visible_Part_Declarative_Items (Self.Node);
      Result : constant Gela.A4G.Elements.Element_Sequence_Access :=
        Self.Context.Create_Element_List (List);
   begin
      return Gela.Element_Sequences.Element_Sequence_Access (Result);
   end Visible_Part;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : aliased Element;
      Visiter : in out Gela.Element_Visiters.Abstract_Visiter'Class)
   is
   begin
      case Asis.Elements.Element_Kind (Self.Node) is
         when Asis.A_Pragma =>
            Visiter.Pragma_Element (Self);

         when Asis.A_Defining_Name =>
            Visiter.Defining_Name (Self);

         when Asis.A_Declaration =>
            case Asis.Elements.Declaration_Kind (Self.Node) is
               when Asis.Not_A_Declaration =>
                  raise Program_Error;

               when Asis.An_Ordinary_Type_Declaration
                  | Asis.A_Task_Type_Declaration
                  | Asis.A_Protected_Type_Declaration
                  | Asis.An_Incomplete_Type_Declaration
                  | Asis.A_Tagged_Incomplete_Type_Declaration
                  | Asis.A_Private_Type_Declaration
                  | Asis.A_Private_Extension_Declaration
                  =>

                  Visiter.Type_Declaration (Self);

               when Asis.A_Subtype_Declaration =>
                  Visiter.Subtype_Declaration (Self);

               when Asis.A_Variable_Declaration
                  | Asis.A_Constant_Declaration
                  | Asis.A_Deferred_Constant_Declaration
                  | Asis.A_Single_Task_Declaration
                  | Asis.A_Single_Protected_Declaration

                  | Asis.An_Integer_Number_Declaration
                  | Asis.A_Real_Number_Declaration

                  | Asis.An_Enumeration_Literal_Specification

                  | Asis.A_Discriminant_Specification
                  | Asis.A_Component_Declaration

                  | Asis.A_Loop_Parameter_Specification
                  | Asis.A_Generalized_Iterator_Specification
                  | Asis.An_Element_Iterator_Specification =>

                  raise Program_Error;

               when Asis.A_Procedure_Declaration
                  | Asis.A_Function_Declaration
                  | Asis.A_Null_Procedure_Declaration
                  | Asis.An_Expression_Function_Declaration
                  | Asis.A_Procedure_Renaming_Declaration
                  | Asis.A_Function_Renaming_Declaration =>

                  Visiter.Subprogram_Declaration (Self);

               when Asis.A_Parameter_Specification =>

                  raise Program_Error;

               when Asis.A_Procedure_Body_Declaration
                  | Asis.A_Function_Body_Declaration =>

                  Visiter.Subprogram_Body_Declaration (Self);

               when Asis.A_Return_Variable_Specification
                  | Asis.A_Return_Constant_Specification =>

                  raise Program_Error;

               when Asis.A_Package_Declaration =>
                  Visiter.Package_Declaration (Self);

               when Asis.A_Package_Body_Declaration
                  | Asis.An_Object_Renaming_Declaration
                  | Asis.An_Exception_Renaming_Declaration
                  | Asis.A_Package_Renaming_Declaration
                  | Asis.A_Generic_Package_Renaming_Declaration =>

                  raise Program_Error;

               when Asis.A_Generic_Procedure_Renaming_Declaration
                  | Asis.A_Generic_Function_Renaming_Declaration =>

                  raise Program_Error;

               when Asis.A_Task_Body_Declaration
                  | Asis.A_Protected_Body_Declaration =>

                  raise Program_Error;

               when Asis.An_Entry_Declaration =>
                  Visiter.Entry_Declaration (Self);

               when Asis.An_Entry_Body_Declaration =>
                  Visiter.Entry_Body_Declaration (Self);

               when Asis.An_Entry_Index_Specification

                  | Asis.A_Procedure_Body_Stub
                  | Asis.A_Function_Body_Stub
                  | Asis.A_Package_Body_Stub
                  | Asis.A_Task_Body_Stub
                  | Asis.A_Protected_Body_Stub

                  | Asis.An_Exception_Declaration
                  | Asis.A_Choice_Parameter_Specification =>

                  raise Program_Error;

               when Asis.A_Generic_Procedure_Declaration
                  | Asis.A_Generic_Function_Declaration =>

                  Visiter.Generic_Subprogram_Declaration (Self);

               when Asis.A_Generic_Package_Declaration
                  | Asis.A_Package_Instantiation
                  | Asis.A_Procedure_Instantiation
                  | Asis.A_Function_Instantiation
                  | Asis.A_Formal_Object_Declaration
                  | Asis.A_Formal_Type_Declaration
                  | Asis.A_Formal_Incomplete_Type_Declaration
                  | Asis.A_Formal_Procedure_Declaration
                  | Asis.A_Formal_Function_Declaration
                  | Asis.A_Formal_Package_Declaration
                  | Asis.A_Formal_Package_Declaration_With_Box =>

                  raise Program_Error;

            end case;

         when Asis.A_Definition =>
            raise Program_Error;
         when Asis.An_Expression =>
            raise Program_Error;
         when Asis.An_Association =>
            raise Program_Error;
         when Asis.A_Statement =>
            raise Program_Error;
         when Asis.A_Path =>
            raise Program_Error;
         when Asis.A_Clause =>
            raise Program_Error;
         when Asis.An_Exception_Handler =>
            raise Program_Error;
         when others =>
            raise Program_Error;
      end case;
   end Visit;

end Gela.A4G.Elements;
