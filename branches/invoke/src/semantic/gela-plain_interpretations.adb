with Gela.Int.Symbol_Interpretations;
with Gela.Int.Visiters;
with Gela.Environments;
with Gela.Defining_Name_Cursors;
with Gela.Solutions.Defining_Names;
with Gela.Solutions.Visiters;

package body Gela.Plain_Interpretations is

   package Get_Int is

      type Visiter is new Gela.Int.Visiters.Visiter with record
         Context : Gela.Contexts.Context_Access;
         Env     : Gela.Semantic_Types.Env_Index;
         Symbol  : Gela.Lexical_Types.Symbol;
      end record;

      overriding procedure Symbol_Interpretation
        (Self  : access Visiter;
         Value : Gela.Int.Symbol_Interpretations.Symbol_Interpretation);

   end Get_Int;

   package body Get_Int is

      ---------------------------
      -- Symbol_Interpretation --
      ---------------------------

      overriding procedure Symbol_Interpretation
        (Self  : access Visiter;
         Value : Gela.Int.Symbol_Interpretations.Symbol_Interpretation) is
      begin
         Self.Symbol := Value.Symbol;
      end Symbol_Interpretation;

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
      use type Gela.Lexical_Types.Symbol;
      use type Gela.Interpretations.Interpretation_Index;
      use type Gela.Interpretations.Interpretation_Set_Index;
      use type Gela.Elements.Defining_Names.Defining_Name_Access;
      V : aliased Get_Int.Visiter :=
        (Context => Self.Context,
         Env     => Env,
         Symbol  => 0);

      S    : Gela.Solutions.Solution_Access;
      Name : Gela.Elements.Defining_Names.Defining_Name_Access;
   begin
      if Set /= 0 then
         Self.Interpretations.Element (Set).Visit (V'Access);
      end if;

      if V.Symbol = 0 then
         Result := 0;
      else
         declare
            X : constant Gela.Environments.Environment_Set_Access :=
              Self.Context.Environment_Set;
            Y : constant Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
              := X.Direct_Visible (Env, V.Symbol);
         begin
            if Y.Has_Element then
               Name := Y.Element;
               S := new Gela.Solutions.Defining_Names.Defining_Name_Solution'
                 (Gela.Solutions.Defining_Names.Create (Name));
               Self.Last_Solution := Self.Last_Solution + 1;
               Result := Self.Last_Solution;
               Self.Solutions.Insert (Result, S);
            end if;
         end;
      end if;
   end Chosen_Interpretation;

   -----------------
   -- Direct_Name --
   -----------------

   overriding procedure Direct_Name
     (Self   : in out Interpretation_Manager;
      Name   : Gela.Lexical_Types.Symbol;
      Result : out Gela.Interpretations.Interpretation_Set_Index)
   is
      use type Gela.Interpretations.Interpretation_Set_Index;
      Value : constant Gela.Int.Interpretation_Access :=
        new Gela.Int.Symbol_Interpretations.Symbol_Interpretation'
          (Gela.Int.Symbol_Interpretations.Create (Name));
   begin
      Self.Last_Int := Self.Last_Int + 1;
      Result := Self.Last_Int;
      Self.Interpretations.Insert (Result, Value);
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

end Gela.Plain_Interpretations;
