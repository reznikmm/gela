------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;
with Gela.Pass_1;
with Gela.Nodes;

package body Gela.Simple_Contexts.Loaders is

   type Symbol_Set_Access is access all Mutables.Symbol_Sets.Symbol_Set;

   ----------------
   -- Get_Loader --
   ----------------

   function Get_Loader
     (Context : Gela.Types.Context_Access) return Loader_Access is
   begin
      return Loader_Access (Context_Access (Context).Loader);
   end Get_Loader;

   ------------------------------
   -- Read_File_And_Supporters --
   ------------------------------

   procedure Read_File_And_Supporters
     (Self : in out Loader;
      Name : League.Strings.Universal_String;
      Text : League.Strings.Universal_String)
   is
      Comp   : Gela.Mutables.Mutable_Compilation_Access;
      Set    : constant Symbol_Set_Access := Self.Context.Symbols'Access;
      Origin : Gela.Types.Unit_Origins;
   begin
      if Self.Context.Finder.Is_Predefined (Name) then
         Origin := Gela.Types.A_Predefined_Unit;
      else
         Origin := Gela.Types.An_Application_Unit;
      end if;

      Comp := Gela.Mutables.Compilations.Create
        (Name    => Name,
         Context => Gela.Types.Context_Access (Self.Context),
         Source  => Text,
         Errors  => Self.Context.Errors,
         Symbols => Gela.Types.Symbol_Set_Access (Set),
         Grammar => Self.Context.Grammar,
         Table   => Self.Context.Table,
         Origin  => Origin);

      Comp.Start;

      if Comp.Root.its = null then
         Self.Context.On_Error.Syntax_Error (Name);
      end if;

      declare
         Pass : Gela.Pass_1.Visiter (Comp);
      begin
         Gela.Nodes.Visitable_Node_Access (Comp.Root.its).Visit
           (Payload => Comp.Root.Payload,
            Visiter => Pass);
      end;
   end Read_File_And_Supporters;

   --------------------
   -- Read_Unit_Body --
   --------------------

   not overriding procedure Read_Unit_Body
     (Self      : in out Loader;
      Full_Name : Gela.Types.Symbol;
      Result    : out Gela.Types.Compilation_Unit;
      Found     : out Boolean)
   is
      use type Gela.Types.Compilation_Unit_Access;
      File_Name : League.Strings.Universal_String;
      Name      : League.Strings.Universal_String;
      Text      : League.Strings.Universal_String;
   begin
      Result := Self.Context.Compilation_Unit_Body (Full_Name);

      if Result.Object /= null then
         Found := True;
         return;
      end if;

      File_Name := Self.Context.Symbols.Value (Full_Name);
      File_Name := Self.Context.Schema.Body_Name (File_Name);

      Self.Context.Finder.Lookup_File (File_Name, Found, Name, Text);

      if Found then
         Self.Read_File_And_Supporters (Name, Text);
         Result := Self.Context.Compilation_Unit_Body (Full_Name);
         Found := Result.Object /= null;
      end if;
   end Read_Unit_Body;

   ---------------------------
   -- Read_Unit_Declaration --
   ---------------------------

   not overriding procedure Read_Unit_Declaration
     (Self      : in out Loader;
      Full_Name : Gela.Types.Symbol;
      Result    : out Gela.Types.Compilation_Unit;
      Found     : out Boolean)
   is
      use type Gela.Types.Compilation_Unit_Access;
      File_Name : League.Strings.Universal_String;
      Name      : League.Strings.Universal_String;
      Text      : League.Strings.Universal_String;
   begin
      Result := Self.Context.Library_Unit_Declaration (Full_Name);

      if Result.Object /= null then
         Found := True;
         return;
      end if;

      File_Name := Self.Context.Symbols.Value (Full_Name);
      File_Name := Self.Context.Schema.Declaration_Name (File_Name);

      Self.Context.Finder.Lookup_File (File_Name, Found, Name, Text);

      if Found then
         Self.Read_File_And_Supporters (Name, Text);
         Result := Self.Context.Library_Unit_Declaration (Full_Name);
         Found := Result.Object /= null;
      end if;
   end Read_Unit_Declaration;

   ----------------------------------
   -- Try_Read_File_And_Supporters --
   ----------------------------------

   procedure Try_Read_File_And_Supporters
     (Self      : in out Loader;
      File_Name : League.Strings.Universal_String)
   is
      Found : Boolean;
      Name  : League.Strings.Universal_String;
      Text  : League.Strings.Universal_String;
   begin
      Self.Context.Finder.Lookup_File (File_Name, Found, Name, Text);

      if not Found then
         Self.Context.On_Error.File_Not_Found (File_Name);
      else
         Self.Read_File_And_Supporters (Name, Text);
      end if;
   end Try_Read_File_And_Supporters;

end Gela.Simple_Contexts.Loaders;
