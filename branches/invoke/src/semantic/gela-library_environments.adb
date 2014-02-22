--  with Gela.Defining_Name_Cursors;
with Gela.Compilation_Units;
with Gela.Elements.Defining_Names;
with Gela.Compilation_Unit_Sets;

package body Gela.Library_Environments is

   Library_Env : constant Gela.Semantic_Types.Env_Index := 1;

   package Library_Cursor is
      type Defining_Name_Cursor is
        new Gela.Defining_Name_Cursors.Defining_Name_Cursor
      with record
         Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      end record;

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean;

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access;

      overriding procedure Next (Self : in out Defining_Name_Cursor);

   end Library_Cursor;

   --------------------
   -- Library_Cursor --
   --------------------

   package body Library_Cursor is

      -----------------
      -- Has_Element --
      -----------------

      overriding function Has_Element
        (Self : Defining_Name_Cursor) return Boolean
      is
         use type Gela.Elements.Defining_Names.Defining_Name_Access;
      begin
         return Self.Name /= null;
      end Has_Element;

      -------------
      -- Element --
      -------------

      overriding function Element
        (Self : Defining_Name_Cursor)
         return Gela.Elements.Defining_Names.Defining_Name_Access
      is
         pragma Unreferenced (Self);
      begin
         return null;
      end Element;

      ----------
      -- Next --
      ----------

      overriding procedure Next (Self : in out Defining_Name_Cursor) is
      begin
         Self.Name := null;
      end Next;

   end Library_Cursor;

   --------------------
   -- Direct_Visible --
   --------------------

   overriding function Direct_Visible
     (Self   : in out Environment_Set;
      Index  : Gela.Semantic_Types.Env_Index;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Defining_Name_Cursors.Defining_Name_Cursor'Class
   is
      use type Gela.Semantic_Types.Env_Index;
      use type Gela.Compilation_Units.Compilation_Unit_Access;
      Unit  : Gela.Compilation_Units.Compilation_Unit_Access;
      Units : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      if Index /= Library_Env then
         raise Constraint_Error;
      end if;

      Units := Self.Context.Library_Unit_Declarations;
      Unit := Units.Find (Symbol);

      if Unit = null then
         Units := Self.Context.Compilation_Unit_Bodies;
         Unit := Units.Find (Symbol);
      end if;

      return Result : Library_Cursor.Defining_Name_Cursor;
   end Direct_Visible;

   -------------------------------
   -- Library_Level_Environment --
   -------------------------------

   overriding procedure Library_Level_Environment
     (Self  : in out Environment_Set;
      Value : out Gela.Semantic_Types.Env_Index)
   is
      pragma Unreferenced (Self);
   begin
      Value := Library_Env;
   end Library_Level_Environment;

end Gela.Library_Environments;
