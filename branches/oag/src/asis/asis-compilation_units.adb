------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-compilation_units.adb 2455 2006-06-24 19:22:06Z maxr $

with Asis.Gela.Strings;
with Asis.Gela.Contexts;
with Asis.Gela.Elements;
with Asis.Gela.Compilations;

package body Asis.Compilation_Units is
   use Asis.Gela;
   use Asis.Gela.Elements;
   use Asis.Gela.Contexts;

   function Has_String
     (Compilation_Unit : Asis.Compilation_Unit;
      Kind             : P.Property_Kinds)
     return Wide_String;

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
   begin
      if Assigned (Compilation_Unit) then
         declare
            Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

            C : constant Compilations.Compilation :=
              Get_Compilation (Compilation_Unit);

            Result : constant Element_Index :=
              Get (C, Unit, P.Can_Be_Main_Program);
         begin
            return Boolean'Val (Result);
         end;
      else
         return False;
      end if;
   end Can_Be_Main_Program;

   --------------------------------------
   -- Compilation_Command_Line_Options --
   --------------------------------------

   function Compilation_Command_Line_Options
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      if Assigned (Compilation_Unit) then
         raise Not_Implemented_Error;
         return "";
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
      Check_Context (The_Context);
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit_List;
   end Compilation_Unit_Bodies;

   ---------------------------
   -- Compilation_Unit_Body --
   ---------------------------

   function Compilation_Unit_Body
     (Name        : in Wide_String;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit is
   begin
      Check_Context (The_Context);
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
   end Compilation_Unit_Body;

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List
   is
   begin
      Check_Context (The_Context);
      return Get_Units (The_Context);
   end Compilation_Units;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body
     (Library_Item : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit
   is
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Body");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   function Corresponding_Body
     (Library_Item : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit
   is
   begin
      Check_Context (The_Context);
      Check_Nil_Unit (Library_Item, "Corresponding_Body");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
   end Corresponding_Body;

   ----------------------------
   -- Corresponding_Children --
   ----------------------------

   function Corresponding_Children
     (Library_Unit : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit_List is
   begin
      Check_Nil_Unit (Library_Unit, "Corresponding_Children");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit_List;
   end Corresponding_Children;

   ----------------------------
   -- Corresponding_Children --
   ----------------------------

   function Corresponding_Children
     (Library_Unit : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      Check_Context (The_Context);
      Check_Nil_Unit (Library_Unit, "Corresponding_Children");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit_List;
   end Corresponding_Children;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Library_Item : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit is
   begin
      Check_Nil_Unit (Library_Item, "Corresponding_Declaration");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
   end Corresponding_Declaration;

   -------------------------------
   -- Corresponding_Declaration --
   -------------------------------

   function Corresponding_Declaration
     (Library_Item : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit is
   begin
      Check_Context (The_Context);
      Check_Nil_Unit (Library_Item, "Corresponding_Declaration");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
   end Corresponding_Declaration;

   --------------------------------------
   -- Corresponding_Parent_Declaration --
   --------------------------------------

   function Corresponding_Parent_Declaration
     (Library_Unit : in Asis.Compilation_Unit)
      return Asis.Compilation_Unit is
   begin
      Check_Nil_Unit (Library_Unit, "Corresponding_Parent_Declaration");
      return Nil_Compilation_Unit;
   end Corresponding_Parent_Declaration;

   --------------------------------------
   -- Corresponding_Parent_Declaration --
   --------------------------------------

   function Corresponding_Parent_Declaration
     (Library_Unit : in Asis.Compilation_Unit;
      The_Context  : in Asis.Context)
      return Asis.Compilation_Unit is
   begin
      Check_Context (The_Context);
      Check_Nil_Unit (Library_Unit, "Corresponding_Parent_Declaration");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
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
      return Nil_Compilation_Unit;
   end Corresponding_Subunit_Parent_Body;

   ---------------------------------------
   -- Corresponding_Subunit_Parent_Body --
   ---------------------------------------

   function Corresponding_Subunit_Parent_Body
     (Subunit     : in Asis.Compilation_Unit;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit
   is
   begin
      Check_Context (The_Context);
      Check_Nil_Unit (Subunit, "Corresponding_Subunit_Parent_Body");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
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
      use Asis.Ada_Environments.Containers;
      List : constant Container_List :=
        Defining_Containers (Enclosing_Context (Compilation_Unit));
   begin
      return List (List'First);
   end Enclosing_Container;

   -----------------------
   -- Enclosing_Context --
   -----------------------

   function Enclosing_Context
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Context is
   begin
      Check_Nil_Unit (Compilation_Unit, "Enclosing_Context");
      return Get_Context (Compilation_Unit);
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
   begin
      if Assigned (Compilation_Unit) then
         return False;
--         return Has_Attribute (Compilation_Unit.all, Attribute);
      else
         return False;
      end if;
   end Has_Attribute;

   ----------------
   -- Has_String --
   ----------------

   function Has_String
     (Compilation_Unit : Asis.Compilation_Unit;
      Kind             : P.Property_Kinds)
     return Wide_String is
   begin
      if Assigned (Compilation_Unit) then
         declare
            Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

            C : constant Compilations.Compilation :=
              Get_Compilation (Compilation_Unit);

            Result : constant Element_Index :=
              Get (C, Unit, Kind);
         begin
            if Result > 0 then
               return Strings.Get (C.Text_Buffer, Positive (Result));
            end if;
         end;
      end if;

      return "";
   end Has_String;

   ----------------------
   -- Is_Body_Required --
   ----------------------

   function Is_Body_Required
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Boolean
   is
   begin
      if Assigned (Compilation_Unit) then
         declare
            Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

            C : constant Compilations.Compilation :=
              Get_Compilation (Compilation_Unit);

            Result : constant Element_Index :=
              Get (C, Unit, P.Is_Body_Required);
         begin
            return Boolean'Val (Result);
         end;
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
   begin
      return Left.Index = Right.Index;
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
      Check_Context (The_Context);
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit;
   end Library_Unit_Declaration;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   function Library_Unit_Declarations
     (The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      Check_Context (The_Context);
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit_List;
   end Library_Unit_Declarations;

   -----------------
   -- Object_Form --
   -----------------

   function Object_Form
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      return Has_String (Compilation_Unit, P.Object_Form);
   end Object_Form;

   -----------------
   -- Object_Name --
   -----------------

   function Object_Name
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      return Has_String (Compilation_Unit, P.Object_Name);
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
      return Nil_Compilation_Unit_List;
   end Subunits;

   --------------
   -- Subunits --
   --------------

   function Subunits
     (Parent_Body : in Asis.Compilation_Unit;
      The_Context : in Asis.Context)
      return Asis.Compilation_Unit_List is
   begin
      Check_Context (The_Context);
      Check_Nil_Unit (Parent_Body, "Subunits");
      raise Not_Implemented_Error;
      return Nil_Compilation_Unit_List;
   end Subunits;

   ---------------
   -- Text_Form --
   ---------------

   function Text_Form
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      return Has_String (Compilation_Unit, P.Text_Form);
   end Text_Form;

   ---------------
   -- Text_Name --
   ---------------

   function Text_Name (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String is
   begin
      return Has_String (Compilation_Unit, P.Text_Name);
   end Text_Name;

   -----------------
   -- Unique_Name --
   -----------------

   function Unique_Name
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Wide_String
   is
   begin
      return Has_String (Compilation_Unit, P.Unique_Name);
   end Unique_Name;

   ----------------
   -- Unit_Class --
   ----------------

   function Unit_Class
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Unit_Classes is
   begin
      if Assigned (Compilation_Unit) then
         declare
            Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

            C : constant Compilations.Compilation :=
              Get_Compilation (Compilation_Unit);

            Result : constant Element_Index :=
              Get (C, Unit, P.Unit_Class);
         begin
            return Unit_Classes'Val (Result);
         end;
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
      return Has_String (Compilation_Unit, P.Unit_Full_Name);
   end Unit_Full_Name;

   ---------------
   -- Unit_Kind --
   ---------------

   function Unit_Kind
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Unit_Kinds is
   begin
      if Assigned (Compilation_Unit) then
         declare
            Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

            C : constant Compilations.Compilation :=
              Get_Compilation (Compilation_Unit);

            Result : constant Element_Index :=
              Get (C, Unit, P.Unit_Kind);
         begin
            return Unit_Kinds'Val (Result);
         end;
      else
         return Not_A_Unit;
      end if;
   end Unit_Kind;

   -----------------
   -- Unit_Origin --
   -----------------

   function Unit_Origin
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Unit_Origins is
   begin
      if Assigned (Compilation_Unit) then
         declare
            Unit : constant Element_Index := Get_Unit_Data (Compilation_Unit);

            C : constant Compilations.Compilation :=
              Get_Compilation (Compilation_Unit);

            Result : constant Element_Index :=
              Get (C, Unit, P.Unit_Origin);
         begin
            return Unit_Origins'Val (Result);
         end;
      else
         return Not_An_Origin;
      end if;
   end Unit_Origin;

end Asis.Compilation_Units;



------------------------------------------------------------------------------
--  Copyright (c) 2006, Maxim Reznik
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
