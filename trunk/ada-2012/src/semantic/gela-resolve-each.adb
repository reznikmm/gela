with Gela.Elements.Subtype_Indications;
with Gela.Plain_Int_Sets.Cursors;
with Gela.Types.Simple;
with Gela.Types.Visitors;

package body Gela.Resolve.Each is

   type Name_As_Expression_Cursor is
     new Gela.Interpretations.Expression_Cursor with
   record
      Name : Gela.Plain_Int_Sets.Cursors.Defining_Name_Cursor;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Tipe : Gela.Semantic_Types.Type_Index := 0;
   end record;

   procedure Step (Self : in out Name_As_Expression_Cursor'Class);

   overriding function Has_Element
     (Self : Name_As_Expression_Cursor) return Boolean;

   overriding procedure Next (Self : in out Name_As_Expression_Cursor);

   overriding function Get_Index
     (Self : Name_As_Expression_Cursor)
       return Gela.Interpretations.Interpretation_Index;

   overriding function Expression_Type
     (Self : Name_As_Expression_Cursor)
       return Gela.Semantic_Types.Type_Index;

   type Join_Cursor is
     new Gela.Interpretations.Expression_Cursor with
   record
      Name : Name_As_Expression_Cursor;
      Exp  : Gela.Plain_Int_Sets.Cursors.Expression_Cursor;
   end record;

   overriding function Has_Element (Self : Join_Cursor) return Boolean;

   overriding procedure Next (Self : in out Join_Cursor);

   overriding function Get_Index
     (Self : Join_Cursor) return Gela.Interpretations.Interpretation_Index;

   overriding function Expression_Type
     (Self : Join_Cursor) return Gela.Semantic_Types.Type_Index;

   procedure Initialize
     (Self : in out Join_Cursor'Class;
      IM   : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index);

   type Prefix_Cursor is new Join_Cursor with record
      Is_Implicit_Dereference   : Boolean := False;
      Implicit_Dereference_Type : Gela.Semantic_Types.Type_Index;
   end record;

   overriding procedure Next (Self : in out Prefix_Cursor);

   overriding function Expression_Type
     (Self : Prefix_Cursor) return Gela.Semantic_Types.Type_Index;

   procedure Step (Self : in out Prefix_Cursor'Class);

   type Prefer_Root_Cursor is
     new Gela.Interpretations.Expression_Cursor with
   record
      Has_Integer_Root : Boolean := False;
      Has_Real_Root    : Boolean := False;
      Root_Cursor      : Join_Cursor;
      Exp_Cursor       : Join_Cursor;
   end record;

   procedure Step (Self : in out Prefer_Root_Cursor'Class);

   overriding function Has_Element
     (Self : Prefer_Root_Cursor) return Boolean;

   overriding procedure Next (Self : in out Prefer_Root_Cursor);

   overriding function Get_Index
     (Self : Prefer_Root_Cursor)
       return Gela.Interpretations.Interpretation_Index;

   overriding function Expression_Type
     (Self : Prefer_Root_Cursor)
       return Gela.Semantic_Types.Type_Index;

   procedure Initialize
     (Self : in out Prefer_Root_Cursor'Class;
      IM   : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index);

   ---------------------
   -- Expression_Type --
   ---------------------

   overriding function Expression_Type
     (Self : Name_As_Expression_Cursor)
      return Gela.Semantic_Types.Type_Index is
   begin
      return Self.Tipe;
   end Expression_Type;

   ---------------------
   -- Expression_Type --
   ---------------------

   overriding function Expression_Type
     (Self : Join_Cursor)
      return Gela.Semantic_Types.Type_Index is
   begin
      if Self.Name.Has_Element then
         return Self.Name.Expression_Type;
      else
         return Self.Exp.Expression_Type;
      end if;
   end Expression_Type;

   ---------------
   -- Get_Index --
   ---------------

   overriding function Get_Index
     (Self : Name_As_Expression_Cursor)
       return Gela.Interpretations.Interpretation_Index is
   begin
      return Self.Name.Get_Index;
   end Get_Index;

   ---------------
   -- Get_Index --
   ---------------

   overriding function Get_Index
     (Self : Join_Cursor) return Gela.Interpretations.Interpretation_Index is
   begin
      if Self.Name.Has_Element then
         return Self.Name.Get_Index;
      else
         return Self.Exp.Get_Index;
      end if;
   end Get_Index;

   ---------------
   -- Get_Index --
   ---------------

   overriding function Get_Index
     (Self : Prefer_Root_Cursor)
       return Gela.Interpretations.Interpretation_Index is
   begin
      if Self.Root_Cursor.Has_Element then
         return Self.Root_Cursor.Get_Index;
      else
         return Self.Exp_Cursor.Get_Index;
      end if;
   end Get_Index;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Name_As_Expression_Cursor) return Boolean is
   begin
      return Self.Name.Has_Element;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element (Self : Join_Cursor) return Boolean is
   begin
      return Self.Name.Has_Element or else Self.Exp.Has_Element;
   end Has_Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Prefer_Root_Cursor) return Boolean is
   begin
      return Self.Root_Cursor.Has_Element or else Self.Exp_Cursor.Has_Element;
   end Has_Element;

   procedure Initialize
     (Self : in out Join_Cursor'Class;
      IM   : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self.Exp := Gela.Plain_Int_Sets.Cursors.Expression_Cursor
        (IM.Expressions (Set).First);

      Self.Name.Name := Gela.Plain_Int_Sets.Cursors.Defining_Name_Cursor
        (IM.Defining_Names (Set).First);

      Self.Name.TM   := TM;
      Self.Name.Env  := Env;
      Self.Name.Tipe := 0;
      Self.Name.Step;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Prefer_Root_Cursor'Class;
      IM   : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index) is
   begin
      Self.Root_Cursor.Initialize (IM, TM, Env, Set);
      Self.Exp_Cursor.Initialize (IM, TM, Env, Set);
      Self.Step;
   end Initialize;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Name_As_Expression_Cursor) is
   begin
      Self.Name.Next;
      Self.Step;
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Join_Cursor) is
   begin
      if Self.Name.Has_Element then
         Self.Name.Next;
      else
         Self.Exp.Next;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Prefer_Root_Cursor) is
   begin
      if Self.Root_Cursor.Has_Element then
         Self.Root_Cursor.Next;
      else
         Self.Exp_Cursor.Next;
      end if;

      Self.Step;
   end Next;

   ----------
   -- Step --
   ----------

   procedure Step (Self : in out Name_As_Expression_Cursor'Class) is
   begin
      Self.Tipe := 0;

      while Self.Name.Has_Element loop
         declare
            Name : constant Gela.Elements.Defining_Names.Defining_Name_Access
              := Self.Name.Defining_Name;
            Decl : constant Gela.Elements.Element_Access :=
              Name.Enclosing_Element;
         begin
            Self.Tipe := Self.TM.Type_Of_Object_Declaration (Self.Env, Decl);
            exit when Self.Tipe not in 0;
            Self.Name.Next;
         end;
      end loop;
   end Step;

   ----------
   -- Step --
   ----------

   procedure Step (Self : in out Prefer_Root_Cursor'Class) is
      TM         : constant Gela.Type_Managers.Type_Manager_Access :=
        Self.Root_Cursor.Name.TM;
      Type_Index : Gela.Semantic_Types.Type_Index;
      Type_View  : Gela.Types.Type_View_Access;
   begin
      --   In the first phase look for root types and return them
      while Self.Root_Cursor.Has_Element loop
         Type_Index := Self.Root_Cursor.Expression_Type;
         Type_View := TM.Get (Type_Index);

         if Type_View in null then
            null;  --  Skip unknown types
         elsif Type_View.Is_Root then
            if Type_View.Is_Signed_Integer then
               Self.Has_Integer_Root := True;
            else
               Self.Has_Real_Root := True;
            end if;

            return;
         end if;

         Self.Root_Cursor.Next;
      end loop;

      --   In the second phase look for other types, if not hidden by root
      while Self.Exp_Cursor.Has_Element loop
         Type_Index := Self.Exp_Cursor.Expression_Type;
         Type_View := TM.Get (Type_Index);

         if Type_View in null then
            null;  --  Skip unknown types
         elsif Type_View.Is_Root then
            null;  --  Skip root types
         elsif Self.Has_Integer_Root and then Type_View.Is_Signed_Integer then
            null;  --  Skip any integer type if we have integer_root
         elsif Self.Has_Real_Root and then Type_View.Is_Real then
            null;  --  Skip any real type if we have real_root
         else
            --  Found other expression type, return it
            return;
         end if;

         Self.Exp_Cursor.Next;
      end loop;
   end Step;

   ----------
   -- Step --
   ----------

   procedure Step (Self : in out Prefix_Cursor'Class) is

      package Type_Visiters is
         type Type_Visitor is new Gela.Types.Visitors.Type_Visitor
         with null record;

         overriding procedure Object_Access_Type
           (Self  : in out Type_Visitor;
            Value : not null Gela.Types.Simple
            .Object_Access_Type_Access);

      end Type_Visiters;

      package body Type_Visiters is

         overriding procedure Object_Access_Type
           (Self  : in out Type_Visitor;
            Value : not null Gela.Types.Simple
            .Object_Access_Type_Access)
         is
            pragma Unreferenced (Self);
            SI : constant Gela.Elements.Subtype_Indications
              .Subtype_Indication_Access := Value.Get_Designated;
            Index : constant Gela.Semantic_Types.Type_Index :=
              Step.Self.Name.TM.Type_From_Subtype_Mark
                (Step.Self.Name.Env, SI.Subtype_Mark);
         begin
            Step.Self.Is_Implicit_Dereference := True;
            Step.Self.Implicit_Dereference_Type := Index;
         end Object_Access_Type;

      end Type_Visiters;

      View    : Gela.Types.Type_View_Access;
      Visiter : Type_Visiters.Type_Visitor;
   begin
      if Self.Has_Element then
         View := Self.Name.TM.Get (Join_Cursor (Self).Expression_Type);
      end if;

      Self.Is_Implicit_Dereference := False;
      View.Visit_If_Assigned (Visiter);
   end Step;

   overriding procedure Next (Self : in out Prefix_Cursor) is
   begin
      if Self.Is_Implicit_Dereference then
         Self.Is_Implicit_Dereference := False;
      else
         Join_Cursor (Self).Next;
         Self.Step;
      end if;
   end Next;

   overriding function Expression_Type
     (Self : Prefix_Cursor) return Gela.Semantic_Types.Type_Index is
   begin
      if Self.Is_Implicit_Dereference then
         return Self.Implicit_Dereference_Type;
      else
         return Join_Cursor (Self).Expression_Type;
      end if;
   end Expression_Type;

   ---------------------
   -- Expression_Type --
   ---------------------

   overriding function Expression_Type
     (Self : Prefer_Root_Cursor)
       return Gela.Semantic_Types.Type_Index is
   begin
      if Self.Root_Cursor.Has_Element then
         return Self.Root_Cursor.Expression_Type;
      else
         return Self.Exp_Cursor.Expression_Type;
      end if;
   end Expression_Type;

   package Join_Iterators is
     new Gela.Plain_Int_Sets.Cursors.Generic_Iterators
     (Cursor      => Gela.Interpretations.Expression_Cursor,
      Next        => Gela.Interpretations.Next,
      Some_Cursor => Join_Cursor,
      Iterators   => Gela.Interpretations.Expression_Iterators);

   package Prefix_Iterators is
     new Gela.Plain_Int_Sets.Cursors.Generic_Iterators
     (Cursor      => Gela.Interpretations.Expression_Cursor,
      Next        => Gela.Interpretations.Next,
      Some_Cursor => Prefix_Cursor,
      Iterators   => Gela.Interpretations.Expression_Iterators);

   package Prefer_Root_Iterators is
     new Gela.Plain_Int_Sets.Cursors.Generic_Iterators
     (Cursor      => Gela.Interpretations.Expression_Cursor,
      Next        => Gela.Interpretations.Next,
      Some_Cursor => Prefer_Root_Cursor,
      Iterators   => Gela.Interpretations.Expression_Iterators);

   -----------------
   -- Prefer_Root --
   -----------------

   function Prefer_Root
     (Self : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Expression_Iterators.Forward_Iterator'Class
   is
   begin
      return Result : Prefer_Root_Iterators.Iterator do
         Result.Cursor.Initialize (Self, TM, Env, Set);
      end return;
   end Prefer_Root;

   ------------
   -- Prefix --
   ------------

   function Prefix
     (Self : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Expression_Iterators.Forward_Iterator'Class
   is
   begin
      return Result : Prefix_Iterators.Iterator do
         Result.Cursor.Initialize (Self, TM, Env, Set);
         Result.Cursor.Step;
      end return;
   end Prefix;

   ----------------
   -- Expression --
   ----------------

   function Expression
     (Self : access Gela.Interpretations.Interpretation_Manager'Class;
      TM   : Gela.Type_Managers.Type_Manager_Access;
      Env  : Gela.Semantic_Types.Env_Index;
      Set  : Gela.Interpretations.Interpretation_Set_Index)
      return Gela.Interpretations.Expression_Iterators.Forward_Iterator'Class
   is
   begin
      return Result : Join_Iterators.Iterator do
         Result.Cursor.Initialize (Self, TM, Env, Set);
      end return;
   end Expression;

end Gela.Resolve.Each;
