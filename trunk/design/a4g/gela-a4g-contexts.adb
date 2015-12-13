with Ada.Strings.Wide_Hash;

with Asis.Ada_Environments;

with Gela.A4G.Symbols;

package body Gela.A4G.Contexts is

   ---------------
   -- Unit_Sets --
   ---------------

   package body Unit_Sets is

      type Forward_Iterator is new Gela.Compilation_Unit_Sets.
        Iterator_Interfaces.Forward_Iterator
      with record
         Context  : Context_Access;
         Body_Set : Boolean := False;
      end record;

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Compilation_Units.Compilation_Unit_Access;

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Compilation_Units.Compilation_Unit_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access;

      function Get_Next
        (Need_Body : Boolean;
         Position  : Unit_Maps.Cursor)
         return Gela.Compilation_Units.Compilation_Unit_Access;

      --------------
      -- Get_Next --
      --------------

      function Get_Next
        (Need_Body : Boolean;
         Position  : Unit_Maps.Cursor)
         return Gela.Compilation_Units.Compilation_Unit_Access
      is
         Pos    : Unit_Maps.Cursor := Position;
         Result : Gela.A4G.Compilation_Units.Compilation_Unit_Access;
      begin
         while Unit_Maps.Has_Element (Pos) loop
            Result := Unit_Maps.Element (Pos);

            if Result.Is_Library_Unit_Declaration = not Need_Body then
               exit;
            end if;

            Unit_Maps.Next (Pos);
         end loop;

         return Gela.Compilation_Units.Compilation_Unit_Access (Result);
      end Get_Next;

      --------------
      -- Is_Empty --
      --------------

      overriding function Is_Empty
        (Self : Compilation_Unit_Set) return Boolean is
      begin
         return Self.Length = 0;
      end Is_Empty;

      ------------
      -- Length --
      ------------

      overriding function Length
        (Self : Compilation_Unit_Set) return Natural
      is
         Result : Natural := Self.Context.Body_Count;
      begin
         if not Self.Need_Body then
            Result := Natural (Self.Context.Units.Length) - Result;
         end if;

         return Result;
      end Length;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Compilation_Units.Compilation_Unit_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access
      is
         Unit : constant Asis.Compilation_Unit :=
           Gela.A4G.Compilation_Units.Compilation_Unit (Position.all).Unit;
         Pos  : Unit_Maps.Cursor := Self.Context.Units.Find (Unit);
      begin
         Unit_Maps.Next (Pos);

         return Get_Next (Self.Body_Set, Pos);
      end Next;

      ----------
      -- Find --
      ----------

      overriding function Find
        (Self   : Compilation_Unit_Set;
         Symbol : not null Gela.Symbols.Symbol_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access
      is
         Unit : Asis.Compilation_Unit;
         Result : Gela.A4G.Compilation_Units.Compilation_Unit_Access;
      begin
         if Self.Need_Body then
            Unit := Asis.Compilation_Units.Compilation_Unit_Body
              (Symbol.Image.To_UTF_16_Wide_String,
               Self.Context.Context);
         else
            Unit := Asis.Compilation_Units.Library_Unit_Declaration
              (Symbol.Image.To_UTF_16_Wide_String,
               Self.Context.Context);
         end if;

         Result := Self.Context.Create_Compilation_Unit (Unit);
         return Gela.Compilation_Units.Compilation_Unit_Access (Result);
      end Find;

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Compilation_Units.Compilation_Unit_Access
      is
         Pos : constant Unit_Maps.Cursor := Self.Context.Units.First;
      begin

         return Get_Next (Self.Body_Set, Pos);
      end First;

      -------------
      -- Iterate --
      -------------

      overriding function Iterate
        (Self : Compilation_Unit_Set)
         return Gela.Compilation_Unit_Sets.Iterator_Interfaces
        .Forward_Iterator'Class is
      begin
         return Forward_Iterator'(Self.Context, Self.Need_Body);
      end Iterate;

      -------------
      -- Context --
      -------------

      overriding function Context
        (Self : Compilation_Unit_Set) return Gela.Contexts.Context_Access is
      begin
         return Self.Context;
      end Context;

   end Unit_Sets;

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   overriding function Compilation_Unit_Bodies
     (Self  : aliased Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access is
   begin
      return Self.Bodies'Access;
   end Compilation_Unit_Bodies;

   -----------------------------
   -- Create_Compilation_Unit --
   -----------------------------

   not overriding function Create_Compilation_Unit
     (Self : access Context;
      Unit : Asis.Compilation_Unit)
      return Gela.A4G.Compilation_Units.Compilation_Unit_Access
   is
      Pos : constant Unit_Maps.Cursor := Self.Units.Find (Unit);

      Result : Gela.A4G.Compilation_Units.Compilation_Unit_Access;
   begin
      if Asis.Compilation_Units.Is_Nil (Unit) then
         return null;
      elsif Unit_Maps.Has_Element (Pos) then
         Result := Unit_Maps.Element (Pos);
      else
         Result := Gela.A4G.Compilation_Units.Create
           (Unit    => Unit,
            Context => Self);

         Self.Units.Insert (Unit, Result);
      end if;

      return Result;
   end Create_Compilation_Unit;

   --------------------
   -- Create_Element --
   --------------------

   not overriding function Create_Element
     (Self    : access Context;
      Element : Asis.Element)
      return Gela.A4G.Elements.Element_Access
   is
      Pos    : constant Element_Maps.Cursor := Self.Elements.Find (Element);

      Result : Gela.A4G.Elements.Element_Access;
   begin
      if Asis.Elements.Is_Nil (Element) then
         return null;
      elsif Element_Maps.Has_Element (Pos) then
         Result := Element_Maps.Element (Pos);
      else
         Result := Gela.A4G.Elements.Create
           (Node    => Element,
            Context => Self);

         Self.Elements.Insert (Element, Result);
      end if;

      return Result;
   end Create_Element;

   -------------------------
   -- Create_Element_List --
   -------------------------

   not overriding function Create_Element_List
     (Self : access Context;
      List : Asis.Element_List)
      return Gela.A4G.Elements.Element_Sequence_Access
   is
      Pos    : Element_List_Maps.Cursor;

      Result : Gela.A4G.Elements.Element_Sequence_Access;
   begin
      if List'Length = 0 then
         return Self.Empty_List;
      end if;

      Pos := Self.Lists.Find (List (List'First));

      if Element_List_Maps.Has_Element (Pos) then
         Result := Element_List_Maps.Element (Pos);
      else
         Result := Gela.A4G.Elements.Create_List
           (Node    => List,
            Context => Self);

         Self.Lists.Insert (List (List'First), Result);
      end if;

      return Result;
   end Create_Element_List;

   -------------------
   -- Create_Symbol --
   -------------------

   not overriding function Create_Symbol
     (Self  : access Context;
      Image : League.Strings.Universal_String)
      return Gela.Symbols.Symbol_Access
   is
      Key : constant League.Strings.Universal_String :=
        Image.To_Simple_Casefold;
      Pos : constant Symbol_Maps.Cursor := Self.Symbols.Find (Key);
      Result : Gela.Symbols.Symbol_Access;
   begin
      if Symbol_Maps.Has_Element (Pos) then
         Result := Symbol_Maps.Element (Pos);
      else
         Result := Gela.A4G.Symbols.Create (Self, Image, Key);
         Self.Symbols.Insert (Key, Result);
      end if;

      return Result;
   end Create_Symbol;

   ----------
   -- Hash --
   ----------

   function Hash
     (Unit : Asis.Compilation_Unit) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Wide_Hash
        (Asis.Compilation_Units.Unique_Name (Unit));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Element : Asis.Element) return Ada.Containers.Hash_Type is
      Image : constant Wide_String := Asis.Elements.Debug_Image (Element);
   begin
      return Ada.Strings.Wide_Hash (Image);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Context;
      Parameters : Wide_String)
   is
   begin
      Asis.Ada_Environments.Associate (Self.Context, Parameters);
      Asis.Ada_Environments.Open (Self.Context);

      Self.Empty_List := Gela.A4G.Elements.Create_List
        (Self'Access, Asis.Nil_Element_List);

      declare
         Ignore : Gela.A4G.Compilation_Units.Compilation_Unit_Access;
         List   : constant Asis.Compilation_Unit_List :=
           Asis.Compilation_Units.Compilation_Units (Self.Context);
      begin
         for J in List'Range loop
            Ignore := Self.Create_Compilation_Unit (List (J));
         end loop;
      end;
   end Initialize;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   overriding function Library_Unit_Declarations
     (Self  : aliased Context)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access is
   begin
      return Self.Specs'Access;
   end Library_Unit_Declarations;

   ------------
   -- Symbol --
   ------------

   overriding function Symbol
     (Self  : aliased Context;
      Image : League.Strings.Universal_String)
      return Gela.Symbols.Symbol_Access
   is
      Key : constant League.Strings.Universal_String :=
        Image.To_Simple_Casefold;
      Pos : constant Symbol_Maps.Cursor := Self.Symbols.Find (Key);
      Result : Gela.Symbols.Symbol_Access;
   begin
      if Symbol_Maps.Has_Element (Pos) then
         Result := Symbol_Maps.Element (Pos);
      else
         Result := null;
         --  Gela.A4G.Symbols.Create (Self'Access, Image, Key);
         --           Self.Symbols.Insert (Key, Result);
      end if;

      return Result;
   end Symbol;

end Gela.A4G.Contexts;
