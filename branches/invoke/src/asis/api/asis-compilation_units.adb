------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-compilation_units.adb 2455 2006-06-24 19:22:06Z maxr $
--  Purpose:
--  Procedural wrapper over Object-Oriented ASIS implementation

with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Ada_Environments.Containers.Internals;

with League.Strings;

with Gela.Contexts; pragma Unreferenced (Gela.Contexts);
with Gela.Compilation_Units;
with Gela.Compilations; pragma Unreferenced (Gela.Compilations);
with Gela.Unit_Containers; pragma Unreferenced (Gela.Unit_Containers);
with Gela.Compilation_Unit_Lists;
pragma Unreferenced (Gela.Compilation_Unit_Lists);

package body Asis.Compilation_Units is

   procedure Check_Nil_Unit
     (Unit : Asis.Compilation_Unit;
      From : Wide_String);

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
      use type Gela.Compilation_Units.Unit_Flag;
   begin
      if Assigned (Compilation_Unit) then
         return (Compilation_Unit.Object.Flags (Compilation_Unit.Payload)
                 and Gela.Compilation_Units.Can_Be_Main) /= 0;
      else
         return False;
      end if;
   end Can_Be_Main_Program;

   --------------------
   -- Check_Nil_Unit --
   --------------------

   procedure Check_Nil_Unit
     (Unit : Asis.Compilation_Unit;
      From : Wide_String) is
   begin
      if not Assigned (Unit) then
         Asis.Implementation.Set_Status
           (Asis.Errors.Value_Error, "Null compilation unit in " & From);
         raise Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit;
      end if;
   end Check_Nil_Unit;

   --------------------------------------
   -- Compilation_Command_Line_Options --
   --------------------------------------

   function Compilation_Command_Line_Options
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
      Compilation : Gela.Types.Compilation_Access;
   begin
      if Assigned (Compilation_Unit) then
         Compilation :=
           Compilation_Unit.Object.Compilation (Compilation_Unit.Payload);
         return Compilation.Compilation_Command_Line_Options
           .To_UTF_16_Wide_String;
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
      if Assigned (The_Context) then
         return To_List (The_Context.Compilation_Unit_Bodies);
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
      return Asis.Compilation_Unit is
   begin
      if Assigned (The_Context) then
         return Asis.Compilation_Unit
           (The_Context.Compilation_Unit_Body
              (League.Strings.From_UTF_16_Wide_String (Name)));
      else
         return Nil_Compilation_Unit;
      end if;
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
      return Asis.Compilation_Unit is
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Body");
      return Asis.Compilation_Unit
        (Library_Item.Object.Corresponding_Body (Library_Item.Payload));
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
      return Asis.Compilation_Unit_List is
   begin
      if Assigned (The_Context) then
         return To_List
           (The_Context.Corresponding_Children
              (Gela.Types.Compilation_Unit (Library_Unit)));
      else
         return Nil_Compilation_Unit_List;
      end if;
   end Corresponding_Children;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Library_Item : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit is
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Declaration");
      return Asis.Compilation_Unit
        (Library_Item.Object.Corresponding_Declaration (Library_Item.Payload));
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
      return Asis.Compilation_Unit is
   begin
      Check_Nil_Unit (Library_Unit, "Corresponding_Parent_Declaration");
      return Asis.Compilation_Unit
        (Library_Unit.Object.Corresponding_Parent_Declaration
           (Library_Unit.Payload));
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
      if Assigned (The_Context) and Assigned (Parent) then
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
   begin
      Check_Nil_Unit (Subunit, "Corresponding_Subunit_Parent_Body");
      return Asis.Compilation_Unit
        (Subunit.Object.Corresponding_Subunit_Parent_Body (Subunit.Payload));
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
      if Assigned (The_Context) and Assigned (Parent) then
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
        (Compilation_Unit.Object.Enclosing_Container
           (Compilation_Unit.Payload));
   end Enclosing_Container;

   -----------------------
   -- Enclosing_Context --
   -----------------------

   function Enclosing_Context
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Context is
   begin
      Check_Nil_Unit (Compilation_Unit, "Enclosing_Context");
      return Asis.Context
        (Compilation_Unit.Object.Enclosing_Container
           (Compilation_Unit.Payload).Parent);
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
        Kind = A_Nonexistent_Body then

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
      use type Gela.Compilation_Units.Unit_Flag;
   begin
      if Assigned (Compilation_Unit) then
         return (Compilation_Unit.Object.Flags (Compilation_Unit.Payload)
                 and Gela.Compilation_Units.Body_Required) /= 0;
      else
         return False;
      end if;
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
      use type Gela.Types.Payload;
      use type Gela.Types.Compilation_Unit_Access;
   begin
      return Left.Object = Right.Object and Left.Payload = Right.Payload;
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
   begin
      if Assigned (The_Context) then
         return Asis.Compilation_Unit
           (The_Context.Library_Unit_Declaration
              (League.Strings.From_UTF_16_Wide_String (Name)));
      else
         return Nil_Compilation_Unit;
      end if;
   end Library_Unit_Declaration;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   function Library_Unit_Declarations
     (The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      if Assigned (The_Context) then
         return To_List (The_Context.Library_Unit_Declarations);
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
      Compilation : Gela.Types.Compilation_Access;
   begin
      if Assigned (Compilation_Unit) then
         Compilation :=
           Compilation_Unit.Object.Compilation (Compilation_Unit.Payload);
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
      return To_List (Parent_Body.Object.Subunits (Parent_Body.Payload));
   end Subunits;

   --------------
   -- Subunits --
   --------------

   function Subunits
     (Parent_Body : in Asis.Compilation_Unit;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      if Assigned (The_Context) then
         return Subunits
           (Compilation_Unit_Body
              (Unit_Full_Name (Parent_Body), The_Context));
      else
         return Nil_Compilation_Unit_List;
      end if;
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
      Compilation : Gela.Types.Compilation_Access;
   begin
      if Assigned (Compilation_Unit) then
         Compilation :=
           Compilation_Unit.Object.Compilation (Compilation_Unit.Payload);
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
         return Compilation_Unit.Object.Unique_Name
           (Compilation_Unit.Payload).To_UTF_16_Wide_String;
      else
         return "";
      end if;
   end Unique_Name;

   ----------------
   -- Unit_Class --
   ----------------

   function Unit_Class
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Unit_Classes is
   begin
      if Assigned (Compilation_Unit) then
         return Asis.Unit_Classes'Val
           (Gela.Types.Unit_Classes'Pos
              (Compilation_Unit.Object.Unit_Class
                 (Compilation_Unit.Payload)) + 1);
      else
         return Not_A_Class;
      end if;
   end Unit_Class;

   --------------------
   -- Unit_Full_Name --
   --------------------

   function Unit_Full_Name
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      if Assigned (Compilation_Unit) then
         return Compilation_Unit.Object.Unit_Full_Name
           (Compilation_Unit.Payload).To_UTF_16_Wide_String;
      else
         return "";
      end if;
   end Unit_Full_Name;

   ---------------
   -- Unit_Kind --
   ---------------

   function Unit_Kind
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Unit_Kinds is
   begin
      if Assigned (Compilation_Unit) then
         return Asis.Unit_Kinds'Val
           (Gela.Types.Unit_Kinds'Pos
              (Compilation_Unit.Object.Unit_Kind
                 (Compilation_Unit.Payload)) + 1);
      else
         return Not_A_Unit;
      end if;
   end Unit_Kind;

   -----------------
   -- Unit_Origin --
   -----------------

   function Unit_Origin
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Unit_Origins
   is
      Compilation : Gela.Types.Compilation_Access;
   begin
      if Assigned (Compilation_Unit) then
         Compilation :=
           Compilation_Unit.Object.Compilation (Compilation_Unit.Payload);
         return Asis.Unit_Origins'Val
           (Gela.Types.Unit_Origins'Pos (Compilation.Origin) + 1);
      else
         return Not_An_Origin;
      end if;
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
