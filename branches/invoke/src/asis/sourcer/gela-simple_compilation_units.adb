------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Simple_Contexts;

package body Gela.Simple_Compilation_Units is

   -----------------
   -- Compilation --
   -----------------

   overriding function Compilation
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Access
   is
      pragma Unreferenced (Payload);
   begin
      return Gela.Types.Compilation_Access (Self.Compilation);
   end Compilation;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   overriding function Corresponding_Body
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      pragma Unreferenced (Payload, Self);
   begin
      return (null, 0);
   end Corresponding_Body;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   overriding function Corresponding_Declaration
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      pragma Unreferenced (Self, Payload);
   begin
      return (null, 0);
   end Corresponding_Declaration;

   --------------------------------------
   -- Corresponding_Parent_Declaration --
   --------------------------------------

   overriding function Corresponding_Parent_Declaration
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      pragma Unreferenced (Self, Payload);
   begin
      return (null, 0);
   end Corresponding_Parent_Declaration;

   ---------------------------------------
   -- Corresponding_Subunit_Parent_Body --
   ---------------------------------------

   overriding function Corresponding_Subunit_Parent_Body
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      pragma Unreferenced (Self, Payload);
   begin
      return (null, 0);
   end Corresponding_Subunit_Parent_Body;

   ------------
   -- Create --
   ------------

   function Create
     (Compilation : Gela.Mutables.Compilations.Compilation_Access;
      Payload     : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      Result : Simple_Compilation_Unit_Access;
   begin
      Result := new Simple_Compilation_Unit'
        (Payload     => Payload,
         Compilation => Compilation);

      return (Gela.Types.Compilation_Unit_Access (Result), Payload);
   end Create;

   -------------------------
   -- Enclosing_Container --
   -------------------------

   overriding function Enclosing_Container
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Container_Access
   is
      pragma Unreferenced (Payload);
   begin
      return Gela.Types.Container_Access
        (Gela.Simple_Contexts.Context_Access (Self.Compilation.Context));
   end Enclosing_Container;

   -----------
   -- Flags --
   -----------

   overriding function Flags
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Compilation_Units.Unit_Flag
   is
      pragma Unreferenced (Payload, Self);
   begin
      return 0;
   end Flags;

   -----------------------
   -- Limited_With_List --
   -----------------------

   overriding function Limited_With_List
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List
   is
      pragma Unreferenced (Self, Payload);
   begin
      return (null, 0);
   end Limited_With_List;

   -------------
   -- Payload --
   -------------

--     function Payload
--       (Self : access Simple_Compilation_Unit) return Gela.Types.Payload is
--     begin
--        return Self.Payload;
--     end Payload;

   --------------
   -- Subunits --
   --------------

   overriding function Subunits
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List
   is
      pragma Unreferenced (Self, Payload);
   begin
      return (null, 0);
   end Subunits;

   -----------------
   -- Unique_Name --
   -----------------

   overriding function Unique_Name
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Payload, Self);
   begin
      return League.Strings.Empty_Universal_String;
   end Unique_Name;

   ----------------
   -- Unit_Class --
   ----------------

   overriding function Unit_Class
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Natural
   is
      pragma Unreferenced (Payload, Self);
   begin
      return 0;
   end Unit_Class;

   --------------------
   -- Unit_Full_Name --
   --------------------

   overriding function Unit_Full_Name
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Payload, Self);
   begin
      return League.Strings.Empty_Universal_String;
   end Unit_Full_Name;

   ---------------
   -- Unit_Kind --
   ---------------

   overriding function Unit_Kind
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Natural
   is
      pragma Unreferenced (Payload, Self);
   begin
      return 0;
   end Unit_Kind;

   -----------------
   -- Unit_Origin --
   -----------------

   overriding function Unit_Origin
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Natural
   is
      pragma Unreferenced (Payload, Self);
   begin
      return 0;
   end Unit_Origin;

   ---------------
   -- With_List --
   ---------------

   overriding function With_List
     (Self    : Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List
   is
      pragma Unreferenced (Payload, Self);
   begin
      return (null, 0);
   end With_List;

end Gela.Simple_Compilation_Units;
