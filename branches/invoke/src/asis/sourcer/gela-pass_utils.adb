------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;
pragma Unreferenced (Gela.Mutables.Compilations);
with Gela.Compilation_Units;
pragma Unreferenced (Gela.Compilation_Units);

with Gela.Simple_Compilation_Units;
with Gela.Simple_Contexts.Loaders;

package body Gela.Pass_Utils is

   function To_Subunit_Kind
     (Value : Gela.Types.Unit_Kinds) return Gela.Types.Unit_Kinds;
   --  Convert subprogram/package unit kind into corresponding subunit kind

   ----------------------
   -- Create_Unit_Body --
   ----------------------

   function Create_Unit_Body
     (Comp       : Gela.Mutables.Mutable_Compilation_Access;
      Full_Name  : Gela.Types.Symbol;
      Unit_Kind  : Gela.Types.Unit_Kinds)
      return Gela.Types.Payload
   is
      use type Gela.Types.Payload;

      Ok         : Boolean;
      Unit       : Gela.Types.Compilation_Unit;
      Payload    : constant Gela.Types.Payload :=
        Gela.Types.Payload (Full_Name) * 2;
      Unit_Class : Gela.Types.Unit_Classes;
      Loader     : constant Gela.Simple_Contexts.Loaders.Loader_Access :=
        Gela.Simple_Contexts.Loaders.Get_Loader (Comp.Context);
   begin
      Loader.Read_Unit_Declaration (Full_Name, Unit, Ok);

      if not Ok then
         if Unit_Kind not in
           Gela.Types.A_Procedure_Body .. Gela.Types.A_Function_Body
         then
            Loader.Context.On_Error.No_Compilation_Unit_Declaration
              (Loader.Context.Symbols.Value (Full_Name));
            --  Can't proceed without unit declaration
            return 0;
         end if;
         Unit_Class := Gela.Types.A_Public_Declaration_And_Body;
      else
         Unit_Class := Unit.Object.Unit_Class (Unit.Payload);

         case Unit_Class is
            when Gela.Types.A_Public_Declaration =>
               Unit_Class := Gela.Types.A_Public_Body;
            when Gela.Types.A_Private_Declaration =>
               Unit_Class := Gela.Types.A_Private_Body;
            when others =>
               raise Constraint_Error;
         end case;
      end if;

      Unit := Gela.Simple_Compilation_Units.Create_Body
        (Compilation => Comp,
         Payload     => Payload,
         Kind        => Unit_Kind,
         Unit_Class  => Unit_Class,
         Full_Name   => Full_Name,
         Declaration => Unit);

      return Payload;
   end Create_Unit_Body;

   -----------------------------
   -- Create_Unit_Declaration --
   -----------------------------

   function Create_Unit_Declaration
     (Comp       : Gela.Mutables.Mutable_Compilation_Access;
      Full_Name  : Gela.Types.Symbol;
      Is_Private : Boolean;
      Unit_Kind  : Gela.Types.Unit_Kinds)
      return Gela.Types.Payload
   is
      use type Gela.Types.Payload;

      Unit       : Gela.Types.Compilation_Unit;
      pragma Unreferenced (Unit);
      Payload    : constant Gela.Types.Payload :=
        Gela.Types.Payload (Full_Name) * 2 + 1;
      Unit_Class : Gela.Types.Unit_Classes;
   begin
      if Is_Private then
         Unit_Class := Gela.Types.A_Private_Declaration;
      else
         Unit_Class := Gela.Types.A_Public_Declaration;
      end if;

      Unit := Gela.Simple_Compilation_Units.Create_Declaration
        (Compilation => Comp,
         Payload     => Payload,
         Kind        => Unit_Kind,
         Unit_Class  => Unit_Class,
         Full_Name   => Full_Name);

      return Payload;
   end Create_Unit_Declaration;

   --------------------
   -- Create_Subunit --
   --------------------

   function Create_Subunit
     (Comp       : Gela.Mutables.Mutable_Compilation_Access;
      Parent     : Gela.Types.Symbol;
      Name       : Gela.Types.Symbol;
      Unit_Kind  : Gela.Types.Unit_Kinds)
      return Gela.Types.Payload
   is
      use type Gela.Types.Payload;

      Ok           : Boolean;
      Full_Name    : Gela.Types.Symbol;
      Unit         : Gela.Types.Compilation_Unit;
      Payload      : Gela.Types.Payload;
      Subunit_Kind : constant Gela.Types.Unit_Kinds :=
        To_Subunit_Kind (Unit_Kind);
      Loader       : constant Gela.Simple_Contexts.Loaders.Loader_Access :=
        Gela.Simple_Contexts.Loaders.Get_Loader (Comp.Context);
   begin
      Loader.Read_Unit_Body (Parent, Unit, Ok);

      if not Ok then
         Loader.Context.On_Error.No_Compilation_Unit_Body
           (Loader.Context.Symbols.Value (Parent));
         --  Can't proceed without unit declaration
         return 0;
      end if;

      Loader.Context.Symbols.Join
        (Prefix   => Parent,
         Selector => Name,
         Result   => Full_Name);

      Payload := Gela.Types.Payload (Full_Name) * 2;

      Unit := Gela.Simple_Compilation_Units.Create_Subunit
        (Compilation => Comp,
         Payload     => Payload,
         Kind        => Subunit_Kind,
         Unit_Class  => Gela.Types.A_Separate_Body,
         Full_Name   => Full_Name,
         Inside      => Unit);

      return Payload;
   end Create_Subunit;

   ---------------------
   -- To_Subunit_Kind --
   ---------------------

   function To_Subunit_Kind
     (Value : Gela.Types.Unit_Kinds)
      return Gela.Types.Unit_Kinds is
   begin
      case Value is
         when Gela.Types.A_Procedure_Body =>
            return Gela.Types.A_Procedure_Body_Subunit;
         when Gela.Types.A_Function_Body_Subunit =>
            return Gela.Types.A_Function_Body_Subunit;
         when Gela.Types.A_Package_Body_Subunit =>
            return Gela.Types.A_Package_Body_Subunit;
         when others =>
            return Value;
      end case;
   end To_Subunit_Kind;

end Gela.Pass_Utils;
