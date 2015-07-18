------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  Purpose:
--  Procedural wrapper over Object-Oriented ASIS implementation

with Asis.Ada_Environments.Containers.Internals;

with League.Strings;
with League.String_Vectors;

with Gela.Compilation_Unit_Sets;
with Gela.Compilation_Units;
with Gela.Compilations;
with Gela.Lexical_Types;
with Gela.Symbol_Sets;

package body Asis.Compilation_Units is

   package U renames Gela.Compilation_Units;

   function To_List
     (Set : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access)
      return Asis.Compilation_Unit_List
        renames Asis.Ada_Environments.Containers.Internals.To_List;

   -------------------------------
   -- Attribute_Value_Delimiter --
   -------------------------------

   function Attribute_Value_Delimiter return Wide_String is
   begin
      return ",";
   end Attribute_Value_Delimiter;

   ----------------------
   -- Attribute_Values --
   ----------------------

   function Attribute_Values
     (Compilation_Unit : in Asis.Compilation_Unit;
      Attribute        : in Wide_String)
      return Wide_String
   is
      pragma Unreferenced (Compilation_Unit);
      pragma Unreferenced (Attribute);
   begin
      return "";
   end Attribute_Values;

   -------------------------
   -- Can_Be_Main_Program --
   -------------------------

   function Can_Be_Main_Program
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Boolean
   is
      pragma Unreferenced (Compilation_Unit);
   begin
      Raise_Not_Implemented ("Can_Be_Main_Program");
      return False;
   end Can_Be_Main_Program;

   --------------------------------------
   -- Compilation_Command_Line_Options --
   --------------------------------------

   function Compilation_Command_Line_Options
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
      Compilation : Gela.Compilations.Compilation_Access;
      Vector      : League.String_Vectors.Universal_String_Vector;
      Result      : League.Strings.Universal_String;
   begin
      if Assigned (Compilation_Unit) then
         Compilation := Compilation_Unit.Data.Compilation;
         Vector := Compilation.Compilation_Command_Line_Options;
         Result := Vector.Join (' ');
         return Result.To_UTF_16_Wide_String;
      else
         return "";
      end if;
   end Compilation_Command_Line_Options;

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   function Compilation_Unit_Bodies
     (The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) then
         return To_List (The_Context.Implementation.Compilation_Unit_Bodies);
      else
         return Nil_Compilation_Unit_List;
      end if;
   end Compilation_Unit_Bodies;

   ---------------------------
   -- Compilation_Unit_Body --
   ---------------------------

   function Compilation_Unit_Body
     (Name        : in Wide_String;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit
   is
      use type Gela.Lexical_Types.Symbol;

      Image   : League.Strings.Universal_String;
      Symbols : Gela.Symbol_Sets.Symbol_Set_Access;
      Symbol  : Gela.Lexical_Types.Symbol;
      Units   : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) then
         Image := League.Strings.From_UTF_16_Wide_String (Name);
         Symbols := The_Context.Implementation.Symbols;
         Symbol := Symbols.Get (Image);

         if Symbol /= Gela.Lexical_Types.No_Symbol then
            Units := The_Context.Implementation.Compilation_Unit_Bodies;
            return (Data => Units.Find (Symbol));
         end if;
      end if;

      return Nil_Compilation_Unit;
   end Compilation_Unit_Body;

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      return Library_Unit_Declarations (The_Context) &
        Compilation_Unit_Bodies (The_Context);
   end Compilation_Units;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body
     (Library_Item : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit
   is
      Result : U.Body_Unit_Access;
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Body");

      if Library_Item.Data.all in U.Library_Unit_Declaration'Class then
         Result := U.Library_Unit_Declaration'Class
           (Library_Item.Data.all).Corresponding_Body;

         return (Data => U.Compilation_Unit_Access (Result));
      end if;

      return Asis.Nil_Compilation_Unit;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body
     (Library_Item : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit is
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Body");
      return Compilation_Unit_Body
        (Unit_Full_Name (Library_Item), The_Context);
   end Corresponding_Body;

   ----------------------------
   -- Corresponding_Children --
   ----------------------------

   function Corresponding_Children
     (Library_Unit : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit_List is
   begin
      Check_Nil_Unit (Library_Unit, "Corresponding_Children");
      return Corresponding_Children
        (Library_Unit, Enclosing_Context (Library_Unit));
   end Corresponding_Children;

   ----------------------------
   -- Corresponding_Children --
   ----------------------------

   function Corresponding_Children
     (Library_Unit : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit_List
   is
      Set : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) then
         if Assigned (Library_Unit) and then
           Library_Unit.Data.all in U.Package_Unit'Class
         then
            Set := U.Package_Unit'Class (Library_Unit.Data.all).
              Corresponding_Childern;

            return To_List (Set);
         end if;
      end if;

      return Nil_Compilation_Unit_List;
   end Corresponding_Children;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Library_Item : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit
   is
      Result : U.Library_Unit_Declaration_Access;
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Declaration");

      if Library_Item.Data.all in U.Body_Unit'Class then
         Result := U.Body_Unit'Class
           (Library_Item.Data.all).Corresponding_Declaration;

         return (Data => U.Compilation_Unit_Access (Result));
      end if;

      return Asis.Nil_Compilation_Unit;
   end Corresponding_Declaration;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Library_Item : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit is
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Declaration");
      return Library_Unit_Declaration
        (Unit_Full_Name (Library_Item), The_Context);
   end Corresponding_Declaration;

   --------------------------------------
   -- Corresponding_Parent_Declaration --
   --------------------------------------

   function Corresponding_Parent_Declaration
     (Library_Unit : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit
   is
      Result : U.Package_Unit_Access;
   begin
      Check_Nil_Unit (Library_Unit, "Corresponding_Parent_Declaration");

      if Library_Unit.Data.all in U.Library_Item'Class then
         Result := U.Library_Item'Class
           (Library_Unit.Data.all).Parent;

         return (Data => U.Compilation_Unit_Access (Result));
      end if;

      return Asis.Nil_Compilation_Unit;
   end Corresponding_Parent_Declaration;

   --------------------------------------
   -- Corresponding_Parent_Declaration --
   --------------------------------------

   function Corresponding_Parent_Declaration
     (Library_Unit : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit
   is
      Parent : constant Asis.Compilation_Unit :=
        Corresponding_Parent_Declaration (Library_Unit);
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) and Assigned (Parent) then
         return Library_Unit_Declaration
           (Unit_Full_Name (Parent), The_Context);
      else
         return Nil_Compilation_Unit;
      end if;
   end Corresponding_Parent_Declaration;

   ---------------------------------------
   -- Corresponding_Subunit_Parent_Body --
   ---------------------------------------

   function Corresponding_Subunit_Parent_Body
     (Subunit : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit
   is
      Result : U.Compilation_Unit_Access;
   begin
      Check_Nil_Unit (Subunit, "Corresponding_Subunit_Parent_Body");

      if Subunit.Data.all in U.Subunit'Class then
         Result := U.Subunit'Class
           (Subunit.Data.all).Corresponding_Subunit_Parent_Body;

         return (Data => Result);
      end if;

      return Asis.Nil_Compilation_Unit;
   end Corresponding_Subunit_Parent_Body;

   ---------------------------------------
   -- Corresponding_Subunit_Parent_Body --
   ---------------------------------------

   function Corresponding_Subunit_Parent_Body
     (Subunit     : in Asis.Compilation_Unit;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit
   is
      Parent : constant Asis.Compilation_Unit :=
        Corresponding_Subunit_Parent_Body (Subunit);
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) and Assigned (Parent) then
         return Compilation_Unit_Body
           (Unit_Full_Name (Parent), The_Context);
      else
         return Nil_Compilation_Unit;
      end if;
   end Corresponding_Subunit_Parent_Body;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      if Assigned (Compilation_Unit) then
         return "Debug_Image";
      else
         return "[null]";
      end if;
   end Debug_Image;

   -------------------------
   -- Enclosing_Container --
   -------------------------

   function Enclosing_Container
     (Compilation_Unit : in Asis.Compilation_Unit)
     return Asis.Ada_Environments.Containers.Container
   is
   begin
      Check_Nil_Unit (Compilation_Unit, "Enclosing_Container");
      return Asis.Ada_Environments.Containers.Internals.Convert
        (Enclosing_Context (Compilation_Unit),
         Compilation_Unit.Data.Container);
   end Enclosing_Container;

   -----------------------
   -- Enclosing_Context --
   -----------------------

   function Enclosing_Context
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Context is
   begin
      Check_Nil_Unit (Compilation_Unit, "Enclosing_Context");
      --  FIXME:
      return (Implementation => Compilation_Unit.Data.Context,
              Name           => League.Strings.Empty_Universal_String,
              Parameters     => League.Strings.Empty_Universal_String,
              Associated     => True);
   end Enclosing_Context;

   ------------
   -- Exists --
   ------------

   function Exists (Compilation_Unit : in Asis.Compilation_Unit) return Boolean
   is
      Kind : constant Asis.Unit_Kinds := Unit_Kind (Compilation_Unit);
   begin
      if Kind = Not_A_Unit or
        Kind = A_Nonexistent_Declaration or
        Kind = A_Nonexistent_Body
      then

         return False;
      else
         return True;
      end if;
   end Exists;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Compilation_Unit : in Asis.Compilation_Unit;
      Attribute        : in Wide_String)
      return Boolean
   is
      pragma Unreferenced (Compilation_Unit);
      pragma Unreferenced (Attribute);
   begin
      return False;
   end Has_Attribute;

   ----------------------
   -- Is_Body_Required --
   ----------------------

   function Is_Body_Required
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Boolean
   is
      pragma Unreferenced (Compilation_Unit);
   begin
      Raise_Not_Implemented ("Is_Body_Required");
      return False;
   end Is_Body_Required;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : in Asis.Compilation_Unit;
      Right : in Asis.Compilation_Unit)
      return Boolean
   is
   begin
      return Unique_Name (Left) = Unique_Name (Right);
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : in Asis.Compilation_Unit;
      Right : in Asis.Compilation_Unit)
      return Boolean
   is
      use type Gela.Compilation_Units.Compilation_Unit_Access;
   begin
      return Left.Data = Right.Data;
   end Is_Identical;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Asis.Compilation_Unit)
      return Boolean is
   begin
      return not Assigned (Right);
   end Is_Nil;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Asis.Compilation_Unit_List)
      return Boolean is
   begin
      return Right'Length = 0;
   end Is_Nil;

   ------------------------------
   -- Library_Unit_Declaration --
   ------------------------------

   function Library_Unit_Declaration
     (Name        : in Wide_String;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit is
      use type Gela.Lexical_Types.Symbol;

      Image   : League.Strings.Universal_String;
      Symbols : Gela.Symbol_Sets.Symbol_Set_Access;
      Symbol  : Gela.Lexical_Types.Symbol;
      Units   : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) then
         Image := League.Strings.From_UTF_16_Wide_String (Name);
         Symbols := The_Context.Implementation.Symbols;
         Symbol := Symbols.Get (Image);

         if Symbol /= Gela.Lexical_Types.No_Symbol then
            Units := The_Context.Implementation.Library_Unit_Declarations;
            return (Data => Units.Find (Symbol));
         end if;
      end if;

      return Nil_Compilation_Unit;
   end Library_Unit_Declaration;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   function Library_Unit_Declarations
     (The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) then
         return To_List (The_Context.Implementation.Library_Unit_Declarations);
      else
         return Nil_Compilation_Unit_List;
      end if;
   end Library_Unit_Declarations;

   -----------------
   -- Object_Form --
   -----------------

   function Object_Form
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
      pragma Unreferenced (Compilation_Unit);
   begin
      return "";
   end Object_Form;

   -----------------
   -- Object_Name --
   -----------------

   function Object_Name
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
      Compilation : Gela.Compilations.Compilation_Access;
   begin
      if Assigned (Compilation_Unit) then
         Compilation := Compilation_Unit.Data.Compilation;
         return Compilation.Object_Name.To_UTF_16_Wide_String;
      else
         return "";
      end if;
   end Object_Name;

   --------------
   -- Subunits --
   --------------

   function Subunits
     (Parent_Body : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit_List
   is
   begin
      Check_Nil_Unit (Parent_Body, "Subunits");
      return Subunits
        (Parent_Body, Enclosing_Context (Parent_Body));
   end Subunits;

   --------------
   -- Subunits --
   --------------

   function Subunits
     (Parent_Body : in Asis.Compilation_Unit;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List
   is
      Set : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      if Asis.Ada_Environments.Is_Open (The_Context) then
         if Assigned (Parent_Body) then
            if Parent_Body.Data.all in U.Body_Unit'Class then
               Set := U.Body_Unit'Class (Parent_Body.Data.all).Subunits;

               return To_List (Set);

            elsif Parent_Body.Data.all in U.Subunit'Class then
               Set := U.Subunit'Class (Parent_Body.Data.all).Subunits;

               return To_List (Set);

            end if;
         end if;
      end if;

      return Nil_Compilation_Unit_List;
   end Subunits;

   ---------------
   -- Text_Form --
   ---------------

   function Text_Form
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
      pragma Unreferenced (Compilation_Unit);
   begin
      return "";
   end Text_Form;

   ---------------
   -- Text_Name --
   ---------------

   function Text_Name
     (Compilation_Unit : in Asis.Compilation_Unit) return Wide_String
   is
      Compilation : Gela.Compilations.Compilation_Access;
   begin
      if Assigned (Compilation_Unit) then
         Compilation := Compilation_Unit.Data.Compilation;
         return Compilation.Text_Name.To_UTF_16_Wide_String;
      else
         return "";
      end if;
   end Text_Name;

   -----------------
   -- Unique_Name --
   -----------------

   function Unique_Name
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      if Assigned (Compilation_Unit) then
         return Unit_Full_Name (Compilation_Unit) & "/" &
           Asis.Unit_Classes'Wide_Image (Unit_Class (Compilation_Unit));
      else
         return "";
      end if;
   end Unique_Name;

   ----------------
   -- Unit_Class --
   ----------------

   function Unit_Class
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Unit_Classes
   is
      pragma Unreferenced (Compilation_Unit);
   begin
      Raise_Not_Implemented ("Unit_Class");
      return Not_A_Class;
   end Unit_Class;

   --------------------
   -- Unit_Full_Name --
   --------------------

   function Unit_Full_Name
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
      use type Gela.Lexical_Types.Symbol;

      Context : Gela.Contexts.Context_Access;
      Image   : League.Strings.Universal_String;
      Symbols : Gela.Symbol_Sets.Symbol_Set_Access;
      Symbol  : Gela.Lexical_Types.Symbol;
   begin
      if Assigned (Compilation_Unit) then
         Symbol := Compilation_Unit.Data.Name;

         if Symbol /= Gela.Lexical_Types.No_Symbol then
            Context := Compilation_Unit.Data.Context;
            Symbols := Context.Symbols;
            Image := Symbols.Image (Symbol);
            return Image.To_UTF_16_Wide_String;
         end if;
      end if;

      return "";
   end Unit_Full_Name;

   ---------------
   -- Unit_Kind --
   ---------------

   function Unit_Kind
     (Compilation_Unit : in Asis.Compilation_Unit) return Asis.Unit_Kinds
   is
      pragma Unreferenced (Compilation_Unit);
   begin
      Raise_Not_Implemented ("Unit_Kind");
      return Not_A_Unit;
   end Unit_Kind;

   -----------------
   -- Unit_Origin --
   -----------------

   function Unit_Origin
     (Compilation_Unit : in Asis.Compilation_Unit) return Asis.Unit_Origins
   is
      pragma Unreferenced (Compilation_Unit);
   begin
      Raise_Not_Implemented ("Unit_Origin");
      return Not_An_Origin;
   end Unit_Origin;

end Asis.Compilation_Units;



------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
