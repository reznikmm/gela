package body Gela.Plain_Compilation_Units is

   -----------------
   -- Compilation --
   -----------------

   overriding function Compilation
     (Self : access Compilation_Unit)
      return Gela.Compilations.Compilation_Access is
   begin
      return Self.Compilation;
   end Compilation;

   ---------------
   -- Container --
   ---------------

   overriding function Container
     (Self : access Compilation_Unit)
      return Gela.Unit_Containers.Unit_Container_Access
   is
   begin
      return Self.Container;
   end Container;

   -------------
   -- Context --
   -------------

   overriding function Context
     (Self : access Compilation_Unit)
      return Gela.Contexts.Context_Access
   is
   begin
      return Self.Compilation.Context;
   end Context;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   overriding function Corresponding_Body
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Library_Unit_Body_Access
   is
   begin
      return Self.Corresponding_Body;
   end Corresponding_Body;

   ----------------------------
   -- Corresponding_Childern --
   ----------------------------

   overriding function Corresponding_Childern
     (Self : access Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
   is
      Result : constant Gela.Plain_Compilation_Unit_Sets.
        Compilation_Unit_Set_Access := Self.Corresponding_Childern'Access;
   begin
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access (Result);
   end Corresponding_Childern;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   overriding function Corresponding_Declaration
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access
   is
   begin
      return Self.Corresponding_Declaration;
   end Corresponding_Declaration;

   ---------------------------------------
   -- Corresponding_Subunit_Parent_Body --
   ---------------------------------------

   overriding function Corresponding_Subunit_Parent_Body
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Compilation_Unit_Access
   is
   begin
      return Self.Corresponding_Subunit_Parent_Body;
   end Corresponding_Subunit_Parent_Body;

   -----------------
   -- Create_Body --
   -----------------

   function Create_Body
     (Node   : Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Parent : Gela.Compilation_Units.Library_Package_Declaration_Access;
      Decl   : Gela.Compilation_Units.Library_Unit_Declaration_Access)
      return Compilation_Unit_Access
   is
      use type Gela.Compilation_Units.Library_Package_Declaration_Access;
      use type Gela.Compilation_Units.Library_Unit_Declaration_Access;

      Tree : constant Gela.Elements.Compilation_Units.Compilation_Unit_Access
        := Gela.Elements.Compilation_Units.Compilation_Unit_Access (Node);
   begin
      return Result : constant Compilation_Unit_Access :=
        new Compilation_Unit'
          (Name                              => Name,
           Tree                              => Tree,
           Container                         => null,
           Compilation                       => Node.Enclosing_Compilation,
           Parent                            => Parent,
           Corresponding_Declaration         => Decl,
           Subunits                          => <>,
           Corresponding_Body                => null,
           Corresponding_Childern            => <>,
           Corresponding_Subunit_Parent_Body => null)
      do
         if Parent /= null then
            Compilation_Unit (Parent.all).Corresponding_Childern.Add
              (Gela.Compilation_Units.Compilation_Unit_Access (Result));
         end if;

         if Decl /= null then
            Compilation_Unit (Decl.all).Corresponding_Body :=
              Gela.Compilation_Units.Library_Unit_Body_Access (Result);
         end if;
      end return;
   end Create_Body;

   ------------------------
   -- Create_Declaration --
   ------------------------

   function Create_Declaration
     (Node   : Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Parent : Gela.Compilation_Units.Library_Package_Declaration_Access)
      return Compilation_Unit_Access
   is
      use type Gela.Compilation_Units.Library_Package_Declaration_Access;

      Tree : constant Gela.Elements.Compilation_Units.Compilation_Unit_Access
        := Gela.Elements.Compilation_Units.Compilation_Unit_Access (Node);
   begin
      return Result : constant Compilation_Unit_Access :=
        new Compilation_Unit'
          (Name                              => Name,
           Tree                              => Tree,
           Container                         => null,
           Compilation                       => Node.Enclosing_Compilation,
           Parent                            => Parent,
           Corresponding_Declaration         => null,
           Subunits                          => <>,
           Corresponding_Body                => null,
           Corresponding_Childern            => <>,
           Corresponding_Subunit_Parent_Body => null)
      do
         if Parent /= null then
            Compilation_Unit (Parent.all).Corresponding_Childern.Add
              (Gela.Compilation_Units.Compilation_Unit_Access (Result));
         end if;
      end return;
   end Create_Declaration;

   --------------------
   -- Create_Subunit --
   --------------------

   function Create_Subunit
     (Node   : Gela.Elements.Subunits.Subunit_Access;
      Name   : Gela.Lexical_Types.Symbol;
      Parent : Compilation_Unit_Access)
      return Compilation_Unit_Access
   is

      Tree : constant Gela.Elements.Compilation_Units.Compilation_Unit_Access
        := Gela.Elements.Compilation_Units.Compilation_Unit_Access (Node);
   begin
      return Result : constant Compilation_Unit_Access :=
        new Compilation_Unit'
          (Name                              => Name,
           Tree                              => Tree,
           Container                         => null,
           Compilation                       => Node.Enclosing_Compilation,
           Parent                            => null,
           Corresponding_Declaration         => null,
           Subunits                          => <>,
           Corresponding_Body                => null,
           Corresponding_Childern            => <>,
           Corresponding_Subunit_Parent_Body =>
             Gela.Compilation_Units.Compilation_Unit_Access (Parent))
      do
         Parent.Subunits.Add
           (Gela.Compilation_Units.Compilation_Unit_Access (Result));
      end return;
   end Create_Subunit;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : access Compilation_Unit)
      return Gela.Lexical_Types.Symbol
   is
   begin
      return Self.Name;
   end Name;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : access Compilation_Unit)
      return Gela.Compilation_Units.Library_Package_Declaration_Access
   is
   begin
      return Self.Parent;
   end Parent;

   --------------
   -- Subunits --
   --------------

   overriding function Subunits
     (Self : access Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
   is
      Result : constant Gela.Plain_Compilation_Unit_Sets.
        Compilation_Unit_Set_Access := Self.Subunits'Access;
   begin
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access (Result);
   end Subunits;

   ----------
   -- Tree --
   ----------

   overriding function Tree
     (Self : access Compilation_Unit)
      return Gela.Elements.Compilation_Units.Compilation_Unit_Access is
   begin
      return Self.Tree;
   end Tree;

end Gela.Plain_Compilation_Units;
