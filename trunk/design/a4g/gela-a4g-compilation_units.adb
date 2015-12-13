with Asis.Compilation_Units;
with Asis.Elements;

with League.Strings;

with Gela.A4G.Contexts;
with Gela.A4G.Elements;

package body Gela.A4G.Compilation_Units is

   -------------------
   -- Children_Sets --
   -------------------

   package body Children_Sets is

      type Forward_Iterator is new Gela.Compilation_Unit_Sets.
        Iterator_Interfaces.Forward_Iterator
      with record
         First : Gela.Compilation_Units.Compilation_Unit_Access;
      end record;

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Compilation_Units.Compilation_Unit_Access;

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Compilation_Units.Compilation_Unit_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access;

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
        (Self : Compilation_Unit_Set) return Natural is
      begin
         return Self.Length;
      end Length;

      ----------
      -- Find --
      ----------

      overriding function Find
        (Self   : Compilation_Unit_Set;
         Symbol : not null Gela.Symbols.Symbol_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access
      is
         use type Gela.Symbols.Symbol_Access;
      begin
         for J in Self.Iterate loop
            if J.Name = Symbol then
               return J;
            end if;
         end loop;

         return null;
      end Find;

      -------------
      -- Iterate --
      -------------

      overriding function Iterate
        (Self : Compilation_Unit_Set)
         return Gela.Compilation_Unit_Sets.Iterator_Interfaces
        .Forward_Iterator'Class is
      begin
         return Forward_Iterator'(First => Self.First);
      end Iterate;

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Compilation_Units.Compilation_Unit_Access is
      begin
         return Self.First;
      end First;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Compilation_Units.Compilation_Unit_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access
      is
         pragma Unreferenced (Self);
      begin
         return Compilation_Unit (Position.all).Next_Child;
      end Next;

      -------------
      -- Context --
      -------------

      overriding function Context
        (Self : Compilation_Unit_Set) return Gela.Contexts.Context_Access is
      begin
         return Self.First.Context;
      end Context;
   end Children_Sets;

   ------------------
   -- Subunit_Sets --
   ------------------

   package body Subunit_Sets is

      type Forward_Iterator is new Gela.Compilation_Unit_Sets.
        Iterator_Interfaces.Forward_Iterator
      with record
         First : Gela.Compilation_Units.Compilation_Unit_Access;
      end record;

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Compilation_Units.Compilation_Unit_Access;

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Compilation_Units.Compilation_Unit_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access;

      -----------
      -- First --
      -----------

      overriding function First
        (Self : Forward_Iterator)
         return Gela.Compilation_Units.Compilation_Unit_Access is
      begin
         return Self.First;
      end First;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self     : Forward_Iterator;
         Position : Gela.Compilation_Units.Compilation_Unit_Access)
         return Gela.Compilation_Units.Compilation_Unit_Access
      is
         pragma Unreferenced (Self);
      begin
         return Compilation_Unit (Position.all).Next_Subunit;
      end Next;

      overriding function Iterate
        (Self : Compilation_Unit_Set)
         return Gela.Compilation_Unit_Sets.Iterator_Interfaces
        .Forward_Iterator'Class is
      begin
         return Forward_Iterator'(First => Self.First);
      end Iterate;

   end Subunit_Sets;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self : aliased Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
   is
   begin
      return Self.Children'Unchecked_Access;
   end Children;

   -------------
   -- Context --
   -------------

   overriding function Context
     (Self : aliased Compilation_Unit)
      return Gela.Contexts.Context_Access is
   begin
      return Gela.Contexts.Context_Access (Self.Context);
   end Context;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   overriding function Corresponding_Body
     (Self : aliased Compilation_Unit)
      return Gela.Compilation_Units.Library_Unit_Body_Access
   is
      Result : constant Compilation_Unit_Access :=
        Self.Context.Create_Compilation_Unit
          (Asis.Compilation_Units.Corresponding_Body (Self.Unit));
   begin
      return Gela.Compilation_Units.Library_Unit_Body_Access (Result);
   end Corresponding_Body;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   overriding function Corresponding_Declaration
     (Self : aliased Compilation_Unit)
      return Gela.Compilation_Units.Library_Unit_Declaration_Access
   is
      Result : constant Compilation_Unit_Access :=
        Self.Context.Create_Compilation_Unit
          (Asis.Compilation_Units.Corresponding_Declaration (Self.Unit));
   begin
      return Gela.Compilation_Units.Library_Unit_Declaration_Access (Result);
   end Corresponding_Declaration;

   ------------
   -- Create --
   ------------

   function Create
     (Unit    : Asis.Compilation_Unit;
      Context : access Gela.A4G.Contexts.Context'Class)
      return Compilation_Unit_Access
   is
      C : constant Context_Access := Context.all'Unchecked_Access;

      Name : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_16_Wide_String
          (Asis.Compilation_Units.Unit_Full_Name (Unit));

      Kind : constant Asis.Unit_Kinds :=
        Asis.Compilation_Units.Unit_Kind (Unit);

      Result : constant Compilation_Unit_Access := new Compilation_Unit'
        (Unit         => Unit,
         Name         => C.Create_Symbol (Name),
         Context      => C,
         Next_Child   => null,
         Next_Subunit => null,
         Children     => <>,
         Subunits     => <>);
   begin
      if Kind in
        Asis.A_Package | Asis.A_Generic_Package | Asis.A_Package_Instance
      then
         --  Add children to list
         declare
            Next : Compilation_Unit_Access;
            List : constant Asis.Compilation_Unit_List :=
              Asis.Compilation_Units.Corresponding_Children (Unit);
         begin
            for J in List'Range loop
               Next := C.Create_Compilation_Unit (List (J));
               Next.Next_Child := Result.Children.First;
               Result.Children.First :=
                 Gela.Compilation_Units.Compilation_Unit_Access (Next);
               Result.Children.Length := Result.Children.Length + 1;
            end loop;
         end;
      elsif Asis.Compilation_Units.Unit_Kind (Unit) in
        Asis.A_Procedure_Body |
        Asis.A_Function_Body |
        Asis.A_Package_Body |
        Asis.A_Procedure_Body_Subunit |
        Asis.A_Function_Body_Subunit |
        Asis.A_Package_Body_Subunit |
        Asis.A_Task_Body_Subunit |
        Asis.A_Protected_Body_Subunit
      then
         --  Add subunits to list
         declare
            Next : Compilation_Unit_Access;
            List : constant Asis.Compilation_Unit_List :=
              Asis.Compilation_Units.Subunits (Unit);
         begin
            for J in List'Range loop
               Next := C.Create_Compilation_Unit (List (J));
               Next.Next_Subunit := Result.Subunits.First;
               Result.Subunits.First :=
                 Gela.Compilation_Units.Compilation_Unit_Access (Next);
               Result.Subunits.Length := Result.Subunits.Length + 1;
            end loop;
         end;
      end if;

      return Result;
   end Create;

   --------------------------
   -- Is_Library_Unit_Body --
   --------------------------

   overriding function Is_Library_Unit_Body
     (Self : aliased Compilation_Unit) return Boolean
   is
   begin
      case Asis.Compilation_Units.Unit_Kind (Self.Unit) is
         when Asis.A_Procedure |
              Asis.A_Function |
              Asis.A_Package |

              Asis.A_Generic_Procedure |
              Asis.A_Generic_Function |
              Asis.A_Generic_Package |

              Asis.A_Procedure_Instance |
              Asis.A_Function_Instance |
              Asis.A_Package_Instance |

              Asis.A_Procedure_Renaming |
              Asis.A_Function_Renaming |
              Asis.A_Package_Renaming |

              Asis.A_Generic_Procedure_Renaming |
              Asis.A_Generic_Function_Renaming |
              Asis.A_Generic_Package_Renaming =>

            return False;

         when Asis.A_Procedure_Body |
              Asis.A_Function_Body |
              Asis.A_Package_Body =>

            return True;

         when Asis.A_Procedure_Body_Subunit |
              Asis.A_Function_Body_Subunit |
              Asis.A_Package_Body_Subunit |
              Asis.A_Task_Body_Subunit |
              Asis.A_Protected_Body_Subunit =>

            return False;

         when others =>

            raise Constraint_Error;

      end case;

   end Is_Library_Unit_Body;

   ---------------------------------
   -- Is_Library_Unit_Declaration --
   ---------------------------------

   overriding function Is_Library_Unit_Declaration
     (Self : aliased Compilation_Unit) return Boolean
   is
   begin
      case Asis.Compilation_Units.Unit_Kind (Self.Unit) is
         when Asis.A_Procedure |
              Asis.A_Function |
              Asis.A_Package |

              Asis.A_Generic_Procedure |
              Asis.A_Generic_Function |
              Asis.A_Generic_Package |

              Asis.A_Procedure_Instance |
              Asis.A_Function_Instance |
              Asis.A_Package_Instance |

              Asis.A_Procedure_Renaming |
              Asis.A_Function_Renaming |
              Asis.A_Package_Renaming |

              Asis.A_Generic_Procedure_Renaming |
              Asis.A_Generic_Function_Renaming |
              Asis.A_Generic_Package_Renaming =>

            return True;

         when Asis.A_Procedure_Body |
              Asis.A_Function_Body |
              Asis.A_Package_Body =>

            return False;

         when Asis.A_Procedure_Body_Subunit |
              Asis.A_Function_Body_Subunit |
              Asis.A_Package_Body_Subunit |
              Asis.A_Task_Body_Subunit |
              Asis.A_Protected_Body_Subunit =>

            return False;

         when others =>

            raise Constraint_Error;

      end case;
   end Is_Library_Unit_Declaration;

   ----------------
   -- Is_Subunit --
   ----------------

   overriding function Is_Subunit
     (Self : aliased Compilation_Unit) return Boolean
   is
   begin
      case Asis.Compilation_Units.Unit_Kind (Self.Unit) is
         when Asis.A_Procedure |
              Asis.A_Function |
              Asis.A_Package |

              Asis.A_Generic_Procedure |
              Asis.A_Generic_Function |
              Asis.A_Generic_Package |

              Asis.A_Procedure_Instance |
              Asis.A_Function_Instance |
              Asis.A_Package_Instance |

              Asis.A_Procedure_Renaming |
              Asis.A_Function_Renaming |
              Asis.A_Package_Renaming |

              Asis.A_Generic_Procedure_Renaming |
              Asis.A_Generic_Function_Renaming |
              Asis.A_Generic_Package_Renaming =>

            return False;

         when Asis.A_Procedure_Body |
              Asis.A_Function_Body |
              Asis.A_Package_Body =>

            return False;

         when Asis.A_Procedure_Body_Subunit |
              Asis.A_Function_Body_Subunit |
              Asis.A_Package_Body_Subunit |
              Asis.A_Task_Body_Subunit |
              Asis.A_Protected_Body_Subunit =>

            return True;

         when others =>

            raise Constraint_Error;

      end case;
   end Is_Subunit;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : aliased Compilation_Unit)
      return Gela.Symbols.Symbol_Access is
   begin
      return Self.Name;
   end Name;


   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : aliased Compilation_Unit)
      return Gela.Compilation_Units.Compilation_Unit_Access
   is
      Result  : Compilation_Unit_Access;
      Context : constant Gela.A4G.Contexts.Context_Access :=
        Gela.A4G.Contexts.Context_Access (Self.Context);
   begin
      if Self.Is_Subunit then
         Result := Context.Create_Compilation_Unit
           (Asis.Compilation_Units.Corresponding_Subunit_Parent_Body
              (Self.Unit));
      else
         Result := Context.Create_Compilation_Unit
           (Asis.Compilation_Units.Corresponding_Parent_Declaration
              (Self.Unit));
      end if;

      return Gela.Compilation_Units.Compilation_Unit_Access (Result);
   end Parent;

   --------------
   -- Subunits --
   --------------

   overriding function Subunits
     (Self : aliased Compilation_Unit)
      return Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access
   is
   begin
      return Self.Subunits'Unchecked_Access;
   end Subunits;

   ----------
   -- Unit --
   ----------

   not overriding function Unit
     (Self : Compilation_Unit) return Asis.Compilation_Unit is
   begin
      return Self.Unit;
   end Unit;

   ----------------------
   -- Unit_Declaration --
   ----------------------

   overriding function Unit_Declaration
     (Self : aliased Compilation_Unit) return Gela.Elements.Element_Access
   is
      Element : constant Asis.Element :=
        Asis.Elements.Unit_Declaration (Self.Unit);
      Result  : constant Gela.A4G.Elements.Element_Access :=
        Self.Context.Create_Element (Element);
   begin
      return Gela.Elements.Element_Access (Result);
   end Unit_Declaration;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : aliased Compilation_Unit;
      Visiter : in out Gela.Compilation_Units.Visiters.Visiter'Class)
   is
   begin
      case Asis.Compilation_Units.Unit_Kind (Self.Unit) is
         when Asis.A_Procedure |
              Asis.A_Function |
              Asis.A_Package |

              Asis.A_Generic_Procedure |
              Asis.A_Generic_Function |
              Asis.A_Generic_Package |

              Asis.A_Procedure_Instance |
              Asis.A_Function_Instance |
              Asis.A_Package_Instance |

              Asis.A_Procedure_Renaming |
              Asis.A_Function_Renaming |
              Asis.A_Package_Renaming |

              Asis.A_Generic_Procedure_Renaming |
              Asis.A_Generic_Function_Renaming |
              Asis.A_Generic_Package_Renaming =>

            Visiter.Library_Unit_Declaration (Self);

         when Asis.A_Procedure_Body |
              Asis.A_Function_Body |
              Asis.A_Package_Body =>

            Visiter.Library_Unit_Body (Self);

         when Asis.A_Procedure_Body_Subunit |
              Asis.A_Function_Body_Subunit |
              Asis.A_Package_Body_Subunit |
              Asis.A_Task_Body_Subunit |
              Asis.A_Protected_Body_Subunit =>

            Visiter.Subunit (Self);

         when others =>

            raise Constraint_Error;

      end case;
   end Visit;

end Gela.A4G.Compilation_Units;
