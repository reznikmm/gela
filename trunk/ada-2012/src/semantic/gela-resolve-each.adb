with Gela.Plain_Int_Sets.Cursors;

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

   package Join_Iterators is
     new Gela.Plain_Int_Sets.Cursors.Generic_Iterators
     (Cursor      => Gela.Interpretations.Expression_Cursor,
      Next        => Gela.Interpretations.Next,
      Some_Cursor => Join_Cursor,
      Iterators   => Gela.Interpretations.Expression_Iterators);

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
         Result.Cursor.Exp :=
           Gela.Plain_Int_Sets.Cursors.Expression_Cursor
             (Self.Expressions (Set).First);

         Result.Cursor.Name.Name :=
           Gela.Plain_Int_Sets.Cursors.Defining_Name_Cursor
             (Self.Defining_Names (Set).First);

         Result.Cursor.Name.TM   := TM;
         Result.Cursor.Name.Env  := Env;
         Result.Cursor.Name.Tipe := 0;
         Result.Cursor.Name.Step;
      end return;
   end Expression;

end Gela.Resolve.Each;
