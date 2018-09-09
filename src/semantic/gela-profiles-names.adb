with Gela.Compilations;
with Gela.Element_Visiters;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Function_Bodies;
with Gela.Elements.Function_Declarations;
with Gela.Elements.Parameter_Specifications;
with Gela.Elements.Procedure_Bodies;
with Gela.Elements.Procedure_Declarations;
with Gela.Type_Managers;

package body Gela.Profiles.Names is

   -------------------------------
   -- Allow_Empty_Argument_List --
   -------------------------------

   overriding function Allow_Empty_Argument_List
     (Self : Profile) return Boolean is
   begin
      return Self.Empty;
   end Allow_Empty_Argument_List;

   ------------
   -- Create --
   ------------

   function Create
     (Env  : Gela.Semantic_Types.Env_Index;
      Name : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Gela.Profiles.Profile'Class
   is
      package Get_Length is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Length : Natural := 0;
         end record;

         procedure Add
           (Self : in out Visiter;
            List : Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Sequence_Access);

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access);

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access);

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access);

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access);

      end Get_Length;

      package body Get_Length is

         procedure Add
           (Self : in out Visiter;
            List : Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Sequence_Access)
         is
            Cursor : Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Sequence_Cursor := List.First;

         begin
            while Cursor.Has_Element loop
               declare
                  Param : constant Gela.Elements.Parameter_Specifications.
                    Parameter_Specification_Access := Cursor.Element;
                  Names : constant Gela.Elements.Defining_Identifiers.
                    Defining_Identifier_Sequence_Access := Param.Names;
                  Pos : Gela.Elements.Defining_Identifiers.
                    Defining_Identifier_Sequence_Cursor := Names.First;
               begin
                  if not Pos.Has_Element then
                     Self.Length := Self.Length + 1;
                  end if;

                  while Pos.Has_Element loop
                     Self.Length := Self.Length + 1;
                     Pos.Next;
                  end loop;

                  Cursor.Next;
               end;
            end loop;
         end Add;

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access) is
         begin
            Self.Add (Node.Parameter_Profile);
         end Function_Body;

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access) is
         begin
            Self.Add (Node.Parameter_Profile);
         end Function_Declaration;

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access) is
         begin
            Self.Add (Node.Parameter_Profile);
         end Procedure_Body;

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access) is
         begin
            Self.Add (Node.Parameter_Profile);
         end Procedure_Declaration;

      end Get_Length;

      Comp : constant Gela.Compilations.Compilation_Access :=
        Name.Enclosing_Compilation;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : access Profile;
            Index  : Natural := 0;
         end record;

         procedure Add
           (Self : in out Visiter;
            List : Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Sequence_Access);

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access);

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access);

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

         procedure Add
           (Self : in out Visiter;
            List : Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Sequence_Access)
         is
            Cursor : Gela.Elements.Parameter_Specifications.
              Parameter_Specification_Sequence_Cursor := List.First;
         begin
            while Cursor.Has_Element loop
               declare
                  Name : Gela.Elements.Defining_Identifiers.
                    Defining_Identifier_Access;
                  Param : constant Gela.Elements.Parameter_Specifications.
                    Parameter_Specification_Access := Cursor.Element;
                  Tipe : constant Gela.Semantic_Types.Type_View_Index :=
                    TM.Type_Of_Object_Declaration
                      (Env, Gela.Elements.Element_Access (Param));
                  Names : constant Gela.Elements.Defining_Identifiers.
                    Defining_Identifier_Sequence_Access := Param.Names;
                  Pos : Gela.Elements.Defining_Identifiers.
                    Defining_Identifier_Sequence_Cursor := Names.First;
               begin
                  if not Pos.Has_Element then
                     Self.Index := Self.Index + 1;
                     Self.Result.Params (Self.Index).Tipe := Tipe;
                  end if;

                  while Pos.Has_Element loop
                     Name := Pos.Element;
                     Self.Index := Self.Index + 1;
                     Self.Result.Params (Self.Index).Name :=
                       Gela.Elements.Defining_Names.Defining_Name_Access
                         (Name);
                     Self.Result.Params (Self.Index).Tipe := Tipe;

                     Pos.Next;
                  end loop;

                  Cursor.Next;
               end;
            end loop;
         end Add;

         overriding procedure Function_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Bodies.
              Function_Body_Access) is
         begin
            Self.Add (Node.Parameter_Profile);

            Self.Result.Funct := True;
            Self.Result.Result :=
              TM.Type_From_Subtype_Mark (Env, Node.Result_Subtype);
         end Function_Body;

         overriding procedure Function_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Function_Declarations.
              Function_Declaration_Access) is
         begin
            Self.Add (Node.Parameter_Profile);

            Self.Result.Funct := True;
            Self.Result.Result :=
              TM.Type_From_Subtype_Mark (Env, Node.Result_Subtype);
         end Function_Declaration;

         overriding procedure Procedure_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Bodies.
              Procedure_Body_Access) is
         begin
            Self.Add (Node.Parameter_Profile);
         end Procedure_Body;

         overriding procedure Procedure_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Procedure_Declarations.
              Procedure_Declaration_Access) is
         begin
            Self.Add (Node.Parameter_Profile);
         end Procedure_Declaration;

      end Get;

      VL : Get_Length.Visiter;

   begin
      Name.Enclosing_Element.Visit (VL);

      return Result : aliased Profile (VL.Length) do
         declare
            V : Get.Visiter;
         begin
            Result.Name := Name;
            V.Result := Result'Unchecked_Access;
            Name.Enclosing_Element.Visit (V);
            Result.Empty := (VL.Length = 0);  --  FIXME
         end;
      end return;
   end Create;

   -----------------
   -- Is_Function --
   -----------------

   overriding function Is_Function (Self : Profile) return Boolean is
   begin
      return Self.Funct;
   end Is_Function;

   ------------
   -- Length --
   ------------

   overriding function Length (Self : Profile) return Natural is
   begin
      return Self.Length;
   end Length;

   -----------------
   -- Return_Type --
   -----------------

   overriding function Return_Type
     (Self  : Profile) return Gela.Semantic_Types.Type_View_Index is
   begin
      return Self.Result;
   end Return_Type;

   --------------
   -- Get_Type --
   --------------

   overriding function Get_Type
     (Self  : Profile;
      Index : Positive)
      return Gela.Semantic_Types.Type_View_Index is
   begin
      return Self.Params (Index).Tipe;
   end Get_Type;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self  : Profile;
      Index : Positive)
      return Gela.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Params (Index).Name;
   end Get_Name;

   ---------------
   -- Get_Index --
   ---------------

   overriding function Get_Index
     (Self   : Profile;
      Symbol : Gela.Lexical_Types.Symbol)
      return Natural
   is
   begin
      raise Constraint_Error with "Unimplemented function Get_Index";
      return 0;
   end Get_Index;

end Gela.Profiles.Names;
