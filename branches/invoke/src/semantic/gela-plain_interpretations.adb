with Gela.Int.Defining_Names;
with Gela.Int.Visiters;
with Gela.Environments;
with Gela.Defining_Name_Cursors;
with Gela.Saves;
with Gela.Solutions.Defining_Names;
with Gela.Solutions.Visiters;

package body Gela.Plain_Interpretations is

   procedure New_Name_Solution
     (Self   : in out Interpretation_Manager'Class;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Save   : Gela.Saves.Save_Access;
      Result : out Gela.Interpretations.Interpretation_Index);
   --  Create Defining_Name_Solution, return its index.

   package Get_Int is

      type Visiter is new Gela.Int.Visiters.Visiter with record
         Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      end record;

      overriding procedure Defining_Name
        (Self  : access Visiter;
         Value : Gela.Int.Defining_Names.Defining_Name);

   end Get_Int;

   package body Get_Int is

      ---------------------------
      -- Name --
      ---------------------------

      overriding procedure Defining_Name
        (Self  : access Visiter;
         Value : Gela.Int.Defining_Names.Defining_Name) is
      begin
         Self.Name := Value.Name;
      end Defining_Name;

   end Get_Int;

   package Get_Name is

      type Visiter is new Gela.Solutions.Visiters.Visiter with record
         Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      end record;

      overriding procedure Defining_Name
        (Self : access Visiter;
         Value : Gela.Solutions.Defining_Names.Defining_Name_Solution);

   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   package body Get_Name is

      -------------------
      -- Defining_Name --
      -------------------

      overriding procedure Defining_Name
        (Self  : access Visiter;
         Value : Gela.Solutions.Defining_Names.Defining_Name_Solution) is
      begin
         Self.Name := Value.Name;
      end Defining_Name;

   end Get_Name;

   ---------------------------
   -- Chosen_Interpretation --
   ---------------------------

   overriding procedure Chosen_Interpretation
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Set    : Gela.Interpretations.Interpretation_Set_Index;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      pragma Unreferenced (Env);
      use type Gela.Lexical_Types.Symbol;
      use type Gela.Interpretations.Interpretation_Set_Index;
      use type Gela.Elements.Defining_Names.Defining_Name_Access;
      X    : Gela.Int.Interpretation_Access;
      V    : aliased Get_Int.Visiter;
   begin
      if Set /= 0 then
         X := Self.Interpretations.Element (Set);
         X.Visit (V'Access);
      end if;

      if V.Name = null then
         Result := 0;
      else
         New_Name_Solution
           (Self,
            V.Name,
            X.Save,
            Result);
      end if;
   end Chosen_Interpretation;

   -----------------
   -- Direct_Name --
   -----------------

   overriding procedure Direct_Name
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
      X : constant Gela.Environments.Environment_Set_Access :=
        Self.Context.Environment_Set;
      Y : constant Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
        := X.Direct_Visible (Env, Name);

      Value : Gela.Int.Interpretation_Access;
   begin
      if Y.Has_Element then
         Value := new Gela.Int.Defining_Names.Defining_Name'
           (Gela.Int.Defining_Names.Create (Y.Element, Save => null));

         Self.Last_Int := Self.Last_Int + 1;
         Result := Self.Last_Int;
         Self.Interpretations.Insert (Result, Value);
      else
         Result := 0;
      end if;
   end Direct_Name;

   -----------------------
   -- Get_Defining_Name --
   -----------------------

   overriding procedure Get_Defining_Name
     (Self   : in out Interpretation_Manager;
      Value  : Gela.Interpretations.Interpretation_Index;
      Result : out Gela.Elements.Defining_Names.Defining_Name_Access)
   is
      use type Gela.Interpretations.Interpretation_Index;
      V : aliased Get_Name.Visiter;
      S : Gela.Solutions.Solution_Access;
   begin
      if Value = 0 then
         Result := null;
      else
         S := Self.Solutions.Element (Value);
         S.Visit (V'Access);
         Result := V.Name;
      end if;

   end Get_Defining_Name;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Gela.Interpretations.Interpretation_Set_Index)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Value);
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Gela.Interpretations.Interpretation_Index)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Value);
   end Hash;

   -----------------------------
   -- Join_Selected_Component --
   -----------------------------

   overriding procedure Join_Selected_Component
     (Self   : in out Interpretation_Manager;
      Env    : Gela.Semantic_Types.Env_Index;
      Prefix : Gela.Interpretations.Interpretation_Set_Index;
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
      use type Gela.Elements.Defining_Names.Defining_Name_Access;
      X : constant Gela.Environments.Environment_Set_Access :=
        Self.Context.Environment_Set;

      Found : aliased Boolean;
      Value : Gela.Int.Interpretation_Access;
      V     : aliased Get_Int.Visiter;
   begin
      if Prefix /= 0 then
         Self.Interpretations.Element (Prefix).Visit (V'Access);
      end if;

      if V.Name /= null then
         declare
            Down : constant Gela.Saves.Save_Access :=
              Self.Interpretations.Element (Prefix).Save;
            Y : constant Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
              := X.Visible (Env, V.Name, Name, Found'Access);
         begin
            if Y.Has_Element then
               Value := new Gela.Int.Defining_Names.Defining_Name'
                 (Gela.Int.Defining_Names.Create
                    (Y.Element,
                     Save => new Gela.Saves.Defining_Name_Save'
                       (Parent => Down,
                        Name   => Y.Element)));

               Self.Last_Int := Self.Last_Int + 1;
               Result := Self.Last_Int;
               Self.Interpretations.Insert (Result, Value);
            else
               Result := 0;
            end if;
         end;
      end if;
   end Join_Selected_Component;

   -----------------------
   -- New_Name_Solution --
   -----------------------

   procedure New_Name_Solution
     (Self   : in out Interpretation_Manager'Class;
      Name   : Gela.Elements.Defining_Names.Defining_Name_Access;
      Save   : Gela.Saves.Save_Access;
      Result : out Gela.Interpretations.Interpretation_Index)
   is
      use type Gela.Interpretations.Interpretation_Index;
      S    : Gela.Solutions.Solution_Access;
   begin
      S := new Gela.Solutions.Defining_Names.Defining_Name_Solution'
        (Gela.Solutions.Defining_Names.Create (Name, Save));
      Self.Last_Solution := Self.Last_Solution + 1;
      Result := Self.Last_Solution;
      Self.Solutions.Insert (Result, S);
   end New_Name_Solution;

   ------------------------------
   -- Split_Selected_Component --
   ------------------------------

   overriding procedure Split_Selected_Component
     (Self     : in out Interpretation_Manager;
      Value    : Gela.Interpretations.Interpretation_Index;
      Prefix   : out Gela.Interpretations.Interpretation_Index;
      Selector : out Gela.Interpretations.Interpretation_Index)
   is
      use type Gela.Interpretations.Interpretation_Index;
      S : Gela.Solutions.Solution_Access;
   begin
      if Value = 0 then
         Prefix := 0;
      else
         S := Self.Solutions.Element (Value);
         New_Name_Solution
           (Self,
            Gela.Saves.Defining_Name_Save (S.Save.all).Name,
            Gela.Saves.Defining_Name_Save (S.Save.all).Parent,
            Prefix);
      end if;

      Selector := Value;
   end Split_Selected_Component;

end Gela.Plain_Interpretations;
