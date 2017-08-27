with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Extensions.Flat_Kinds;

with Gela.Element_Visiters;
with Gela.Elements.Association_Lists;
with Gela.Elements.Associations;
with Gela.Elements.Function_Calls;
with Gela.Elements.Composite_Constraints;
with Gela.Elements.Procedure_Call_Statements;
with Gela.Elements.Record_Aggregates;

package body Asis is

   ----------------------------------
   -- Assert_Inappropriate_Element --
   ----------------------------------

   procedure Assert_Inappropriate_Element (Ok : Boolean; From : Wide_String) is
   begin
      if not Ok then
         Asis.Implementation.Set_Status
           (Asis.Errors.Value_Error, "Inappropriate element in " & From);
         raise Asis.Exceptions.ASIS_Inappropriate_Element;
      end if;
   end Assert_Inappropriate_Element;

   --------------
   -- Assigned --
   --------------

   function Assigned (Unit : in Asis.Compilation_Unit) return Boolean is
      use type Gela.Compilation_Units.Compilation_Unit_Access;
   begin
      return Unit.Data /= null;
   end Assigned;

   --------------
   -- Assigned --
   --------------

   function Assigned (Element : in Asis.Element) return Boolean is
      use type Gela.Elements.Element_Access;
   begin
      return Element.Data /= null;
   end Assigned;

   --------------
   -- Auxilary --
   --------------

   function Auxilary (Element : in Asis.Element) return Boolean is
      package Get is
         type Flag is (Is_Association, Is_Function_Call, Is_Association_List);
         type Flag_Array is array (Flag) of Boolean;
         None : constant Flag_Array := (others => False);

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Boolean := False;
            Flags : Flag_Array := None;
         end record;

         overriding procedure Association
           (Self : in out Visiter;
            Node : not null Gela.Elements.Associations.Association_Access);

         overriding procedure Association_List
           (Self : in out Visiter;
            Node : not null Gela.Elements.Association_Lists.
              Association_List_Access);

         overriding procedure Function_Call
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Calls.
              Function_Call_Access);

         overriding procedure Composite_Constraint
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access);

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access);

         overriding procedure Record_Aggregate
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Aggregates.
              Record_Aggregate_Access);
      end Get;

      package body Get is

         overriding procedure Association
           (Self : in out Visiter;
            Node : not null Gela.Elements.Associations.Association_Access) is
         begin
            if Self.Flags = None then
               Self.Flags (Is_Association) := True;
               Node.Enclosing_Element.Visit (Self);
            end if;
         end Association;

         overriding procedure Function_Call
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Calls.
              Function_Call_Access)
         is
            Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
              Asis.Extensions.Flat_Kinds.Flat_Kind
                ((Data => Gela.Elements.Element_Access (Node)));
         begin
            if Self.Flags = None then
               Self.Flags (Is_Function_Call) := True;
               Node.Enclosing_Element.Visit (Self);
            elsif Self.Flags (Is_Association) then
               Self.Result := Kind in
                 Asis.Extensions.Flat_Kinds.An_Indexed_Component
                 | Asis.Extensions.Flat_Kinds.A_Type_Conversion;
            else
               Self.Result := Self.Flags (Is_Association_List);
            end if;
         end Function_Call;

         overriding procedure Composite_Constraint
           (Self : in out Visiter;
            Node : not null Gela.Elements.Composite_Constraints.
              Composite_Constraint_Access)
         is
            use type Asis.Extensions.Flat_Kinds.Element_Flat_Kind;

            Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
              Asis.Extensions.Flat_Kinds.Flat_Kind
                ((Data => Gela.Elements.Element_Access (Node)));
         begin
            if Kind = Asis.Extensions.Flat_Kinds.An_Index_Constraint then
               Self.Result := Self.Flags (Is_Association);
            else
               Self.Result := False;
            end if;
         end Composite_Constraint;

         overriding procedure Procedure_Call_Statement
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Call_Statements.
              Procedure_Call_Statement_Access)
         is
            pragma Unreferenced (Node);
         begin
            Self.Result := Self.Flags (Is_Function_Call);
         end Procedure_Call_Statement;

         overriding procedure Association_List
           (Self : in out Visiter;
            Node : not null Gela.Elements.Association_Lists.
              Association_List_Access) is
         begin
            Self.Flags (Is_Association_List) := True;
            Node.Enclosing_Element.Visit (Self);
         end Association_List;

         overriding procedure Record_Aggregate
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Aggregates.
              Record_Aggregate_Access) is
         begin
            if Self.Flags = None then
               Node.Enclosing_Element.Visit (Self);
            else
               Self.Result := Self.Flags (Is_Association_List)
                 and not Self.Flags (Is_Association);
            end if;
         end Record_Aggregate;

      end Get;

      V : Get.Visiter;
   begin
      Element.Data.Visit (V);
      return V.Result;
   end Auxilary;

   --------------------
   -- Check_Nil_Unit --
   --------------------

   procedure Check_Nil_Unit
     (Unit : Asis.Compilation_Unit;
      From : Wide_String) is
   begin
      if not Assigned (Unit) then
         Asis.Implementation.Set_Status
           (Asis.Errors.Value_Error, "Null compilation unit in " & From);
         raise Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit;
      end if;
   end Check_Nil_Unit;

   -----------------------
   -- Check_Nil_Element --
   -----------------------

   procedure Check_Nil_Element
     (Element : Asis.Element;
      From    : Wide_String) is
   begin
      if not Assigned (Element) then
         Asis.Implementation.Set_Status
           (Asis.Errors.Value_Error, "Null element in " & From);
         raise Asis.Exceptions.ASIS_Inappropriate_Element;
      end if;
   end Check_Nil_Element;

   ---------------------------
   -- Raise_Not_Implemented --
   ---------------------------

   procedure Raise_Not_Implemented (From : Wide_String) is
   begin
      Asis.Implementation.Set_Status
        (Asis.Errors.Not_Implemented_Error, "Not_Implemented:" & From);
      raise Asis.Exceptions.ASIS_Failed;
   end Raise_Not_Implemented;

   -------------
   -- To_List --
   -------------

   function To_List
     (X : Gela.Elements.Element_Sequence_Access) return Asis.Element_List
   is
      Result : Asis.Element_List (1 .. ASIS_Natural (X.Length));
      Cursor : Gela.Elements.Element_Sequence_Cursor := X.First;
   begin
      for J in Result'Range loop
         Result (J) := (Data => Cursor.Element);
         Cursor.Next;
      end loop;

      return Result;
   end To_List;

end Asis;
