------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Simple_Contexts;
with Gela.Mutables.Compilations;
pragma Unreferenced (Gela.Mutables.Compilations);

package body Gela.Simple_Compilation_Units is

   function Context
     (Self : access Simple_Compilation_Unit'Class)
      return Gela.Simple_Contexts.Context_Access;

   -----------------
   -- Compilation --
   -----------------

   overriding function Compilation
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Access
   is
      pragma Unreferenced (Payload);
   begin
      return Gela.Types.Compilation_Access (Self.Compilation);
   end Compilation;

   -------------
   -- Context --
   -------------

   function Context
     (Self : access Simple_Compilation_Unit'Class)
      return Gela.Simple_Contexts.Context_Access is
   begin
      return Gela.Simple_Contexts.Context_Access (Self.Compilation.Context);
   end Context;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   overriding function Corresponding_Body
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      pragma Unreferenced (Payload);
   begin
      return Self.Corresponding_Body;
   end Corresponding_Body;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   overriding function Corresponding_Declaration
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      pragma Unreferenced (Payload);
   begin
      return Self.Corresponding_Declaration;
   end Corresponding_Declaration;

   --------------------------------------
   -- Corresponding_Parent_Declaration --
   --------------------------------------

   overriding function Corresponding_Parent_Declaration
     (Self    : access Simple_Compilation_Unit;
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
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit
   is
      pragma Unreferenced (Payload);
   begin
      return Self.Corresponding_Subunit_Parent_Body;
   end Corresponding_Subunit_Parent_Body;

   ------------
   -- Create --
   ------------

   function Create
     (Compilation : Gela.Mutables.Mutable_Compilation_Access;
      Payload     : Gela.Types.Payload;
      Kind        : Gela.Types.Unit_Kinds;
      Unit_Class  : Gela.Types.Unit_Classes;
      Origin      : Gela.Types.Unit_Origins;
      Full_Name   : Gela.Types.Symbol)
      return Gela.Types.Compilation_Unit
   is
      Result : Simple_Compilation_Unit_Access;
   begin
      Result := new Simple_Compilation_Unit'
        (Payload     => Payload,
         Compilation => Compilation,
         Kind        => Kind,
         Unit_Class  => Unit_Class,
         Origin      => Origin,
         Full_Name   => Full_Name,
         Corresponding_Declaration => <>,
         Corresponding_Body        => <>,
         Corresponding_Subunit_Parent_Body => <>,
         Next_Unit   => (null, 0),
         Subunits    => <>);

      return (Gela.Types.Compilation_Unit_Access (Result), Payload);
   end Create;

   -----------------
   -- Create_Body --
   -----------------

   function Create_Body
     (Compilation : Gela.Mutables.Mutable_Compilation_Access;
      Payload     : Gela.Types.Payload;
      Kind        : Gela.Types.Unit_Kinds;
      Unit_Class  : Gela.Types.Unit_Classes;
      Origin      : Gela.Types.Unit_Origins;
      Full_Name   : Gela.Types.Symbol;
      Declaration : Gela.Types.Compilation_Unit)
      return Gela.Types.Compilation_Unit
   is
      use type Gela.Types.Compilation_Unit_Access;

      Temp : Gela.Types.Compilation_Unit;
      Unit : Simple_Compilation_Unit_Access;
   begin
      Temp := Create
        (Compilation => Compilation,
         Payload     => Payload,
         Kind        => Kind,
         Unit_Class  => Unit_Class,
         Origin      => Origin,
         Full_Name   => Full_Name);

      Unit := Simple_Compilation_Unit_Access (Temp.Object);
      Unit.Corresponding_Declaration := Declaration;

      if Declaration.Object /= null then
         Unit := Simple_Compilation_Unit_Access (Declaration.Object);
         Unit.Corresponding_Body := Temp;
      end if;

      return Temp;
   end Create_Body;

   --------------------
   -- Create_Subunit --
   --------------------

   function Create_Subunit
     (Compilation : Gela.Mutables.Mutable_Compilation_Access;
      Payload     : Gela.Types.Payload;
      Kind        : Gela.Types.Unit_Kinds;
      Unit_Class  : Gela.Types.Unit_Classes;
      Origin      : Gela.Types.Unit_Origins;
      Full_Name   : Gela.Types.Symbol;
      Inside      : Gela.Types.Compilation_Unit)
     return Gela.Types.Compilation_Unit
   is
      use type Gela.Types.Compilation_Unit_Access;

      Temp : Gela.Types.Compilation_Unit;
      Unit : Simple_Compilation_Unit_Access;
   begin
      Temp := Create
        (Compilation => Compilation,
         Payload     => Payload,
         Kind        => Kind,
         Unit_Class  => Unit_Class,
         Origin      => Origin,
         Full_Name   => Full_Name);

      Unit := Simple_Compilation_Unit_Access (Temp.Object);
      Unit.Corresponding_Subunit_Parent_Body := Inside;

      Unit := Simple_Compilation_Unit_Access (Inside.Object);
      Unit.Subunits.Append (Temp);

      return Temp;
   end Create_Subunit;

   -------------------------
   -- Enclosing_Container --
   -------------------------

   overriding function Enclosing_Container
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Container_Access
   is
      pragma Unreferenced (Payload);
   begin
      return Gela.Types.Container_Access (Context (Self));
   end Enclosing_Container;

   -----------
   -- Flags --
   -----------

   overriding function Flags
     (Self    : access Simple_Compilation_Unit;
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
     (Self    : access Simple_Compilation_Unit;
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
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List
   is
      pragma Unreferenced (Payload);
   begin
      return (Gela.Types.Compilation_Unit_List_Access'(Self.Subunits'Access),
              0);
   end Subunits;

   -----------------
   -- Unique_Name --
   -----------------

   overriding function Unique_Name
     (Self    : access Simple_Compilation_Unit;
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
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Unit_Classes
   is
      pragma Unreferenced (Payload);
   begin
      return Self.Unit_Class;
   end Unit_Class;

   --------------------
   -- Unit_Full_Name --
   --------------------

   overriding function Unit_Full_Name
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Payload);
   begin
      return Context (Self).Symbols.Value (Self.Full_Name);
   end Unit_Full_Name;

   ---------------
   -- Unit_Kind --
   ---------------

   overriding function Unit_Kind
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Unit_Kinds
   is
      pragma Unreferenced (Payload);
   begin
      return Self.Kind;
   end Unit_Kind;

   -----------------
   -- Unit_Origin --
   -----------------

   overriding function Unit_Origin
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Unit_Origins
   is
      pragma Unreferenced (Payload);
   begin
      return Self.Origin;
   end Unit_Origin;

   ---------------
   -- With_List --
   ---------------

   overriding function With_List
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_List
   is
      pragma Unreferenced (Payload, Self);
   begin
      return (null, 0);
   end With_List;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit is
   begin
      return (Gela.Types.Compilation_Unit_Access (Self), Payload);
   end Element;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Self    : access Simple_Compilation_Unit;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor
   is
      pragma Unreferenced (Payload);
   begin
      return
        (Gela.Types.Compilation_Unit_Cursor_Access (Self.Next_Unit.Object),
         Self.Next_Unit.Payload);
   end Next;

   -----------------
   -- Units_Count --
   -----------------

   overriding function Units_Count
     (Self    : access Simple_Unit_List;
      Payload : Gela.Types.Payload)
      return Natural
   is
      pragma Unreferenced (Payload);
   begin
      return Self.Count;
   end Units_Count;

   -----------
   -- First --
   -----------

   overriding function First
     (Self    : access Simple_Unit_List;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Unit_Cursor
   is
      pragma Unreferenced (Payload);
   begin
      return
        (Gela.Types.Compilation_Unit_Cursor_Access (Self.Head.Object),
         Self.Head.Payload);
   end First;

   ------------
   -- Append --
   ------------

   not overriding procedure Append
     (Self    : access Simple_Unit_List;
      Unit    : Gela.Types.Compilation_Unit)
   is
      Temp : Simple_Compilation_Unit_Access;
   begin
      Temp := Simple_Compilation_Unit_Access (Self.Head.Object);
      Temp.Next_Unit := Self.Head;
      Self.Head := Unit;
   end Append;
end Gela.Simple_Compilation_Units;