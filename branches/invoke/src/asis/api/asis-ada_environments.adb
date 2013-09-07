------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  Purpose:
--  Procedural wrapper over Object-Oriented ASIS implementation

with Ada.Exceptions;
with Ada.Characters.Handling;

with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

with League.Strings;

with Gela.Asis_Context_Fabric;
with Gela.Compilations;
pragma Unreferenced (Gela.Compilations);
with Gela.Contexts;
with Gela.Errors;
with Gela.Types;
with Ada.Unchecked_Deallocation;

package body Asis.Ada_Environments is

   package Error_Handler is
      type Handler is new Gela.Errors.Error_Handler with null record;

      overriding procedure File_Not_Found
        (Self      : access Handler;
         File_Name : League.Strings.Universal_String);
      --  Can't lookup file passed in parameters

      overriding procedure No_Compilation_Unit_Body
        (Self      : access Handler;
         Unit_Name : League.Strings.Universal_String);
      --  Body of a unit not provided and not found.

      overriding procedure No_Compilation_Unit_Declaration
        (Self      : access Handler;
         Unit_Name : League.Strings.Universal_String);
      --  Declaration of a unit not provided and not found.

      overriding procedure Not_In_NFKC_Warning
        (Self        : access Handler;
         Compilation : Gela.Types.Compilation_Access);
      --  Text of compilation is not in Normalization Form KC. ARM 4.1/3

      overriding procedure Syntax_Error
        (Self      : access Handler;
         File_Name : League.Strings.Universal_String);
      --  Syntax error while parsing file

      overriding procedure Singe_File_Expected (Self : access Handler);
      --  Only one file should be passed in parameters
   end Error_Handler;

   function Assigned (Self : Asis.Context) return Boolean;

   On_Error : aliased Error_Handler.Handler;

   --------------
   -- Assigned --
   --------------

   function Assigned (Self : Asis.Context) return Boolean is
      use type Gela.Types.Context_Access;
   begin
      return Gela.Types.Context_Access (Self) /= null;
   end Assigned;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (The_Context : in out Asis.Context;
      Name        : in     Wide_String;
      Parameters  : in     Wide_String := Default_Parameters)
  is
   begin
      if not Implementation.Is_Initialized
         or Implementation.Is_Finalized
      then
         Implementation.Set_Status
           (Status    => Asis.Errors.Initialization_Error,
            Diagnosis => "ASIS is not initialized");

         raise Exceptions.ASIS_Failed;
      end if;

      if Is_Open (The_Context) then
         Implementation.Set_Status (
            Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has alredy been opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;

      begin
         if not Assigned (The_Context) then
            Gela.Asis_Context_Fabric.Get.Create_Context
              (On_Error'Access,
               Gela.Types.Context_Access (The_Context));
         end if;

         The_Context.Associate
           (Name       => League.Strings.From_UTF_16_Wide_String (Name),
            Parameters => League.Strings.From_UTF_16_Wide_String (Parameters));
      end;
   end Associate;

   -----------
   -- Close --
   -----------

   procedure Close (The_Context : in out Asis.Context) is
   begin
      if not Is_Open (The_Context) then
         Implementation.Set_Status (
            Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context is not opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;
      The_Context.Close;
   end Close;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (The_Context : in Asis.Context) return Wide_String is
   begin
      if not Assigned (The_Context) then
         return "[null]";
      else
         return The_Context.Debug_Image.To_UTF_16_Wide_String;
      end if;
   end Debug_Image;

   ------------------
   -- Default_Name --
   ------------------

   function Default_Name return Wide_String is
   begin
      return "";
   end Default_Name;

   ------------------------
   -- Default_Parameters --
   ------------------------

   function Default_Parameters return Wide_String is
   begin
      return "";
   end Default_Parameters;

   ----------------
   -- Dissociate --
   ----------------

   type Context_Access is access all Gela.Contexts.Context'Class;

   procedure Dissociate (The_Context : in out Asis.Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Gela.Contexts.Context'Class, Context_Access);
   begin
      if Assigned (The_Context) then
         The_Context.Dissociate;
         Free (Context_Access (The_Context));
      end if;
   end Dissociate;

   -------------------
   -- Error_Handler --
   -------------------

   package body Error_Handler is

      --------------------
      -- File_Not_Found --
      --------------------

      overriding procedure File_Not_Found
        (Self      : access Handler;
         File_Name : League.Strings.Universal_String)
      is
         pragma Unreferenced (Self);
      begin
         Implementation.Set_Status
           (Status    => Asis.Errors.Parameter_Error,
            Diagnosis => "File not found:" & File_Name.To_UTF_16_Wide_String);

         raise Asis.Exceptions.ASIS_Failed;
      end File_Not_Found;

      ------------------------------
      -- No_Compilation_Unit_Body --
      ------------------------------

      overriding procedure No_Compilation_Unit_Body
        (Self      : access Handler;
         Unit_Name : League.Strings.Universal_String)
      is
         pragma Unreferenced (Self);
      begin
         Implementation.Set_Status
           (Status    => Asis.Errors.Parameter_Error,
            Diagnosis => "Body of a unit not provided and not found:" &
              Unit_Name.To_UTF_16_Wide_String);

         raise Asis.Exceptions.ASIS_Failed;
      end No_Compilation_Unit_Body;

      -------------------------------------
      -- No_Compilation_Unit_Declaration --
      -------------------------------------

      overriding procedure No_Compilation_Unit_Declaration
        (Self      : access Handler;
         Unit_Name : League.Strings.Universal_String)
      is
         pragma Unreferenced (Self);
      begin
         Implementation.Set_Status
           (Status    => Asis.Errors.Parameter_Error,
            Diagnosis => "Declaration of a unit not provided and not found:" &
              Unit_Name.To_UTF_16_Wide_String);

         raise Asis.Exceptions.ASIS_Failed;
      end No_Compilation_Unit_Declaration;

      -------------------------
      -- Not_In_NFKC_Warning --
      -------------------------

      overriding procedure Not_In_NFKC_Warning
        (Self        : access Handler;
         Compilation : Gela.Types.Compilation_Access)
      is
         pragma Unreferenced (Self);
      begin
         Implementation.Set_Status
           (Status    => Asis.Errors.Data_Error,
            Diagnosis =>
              "Text of compilation is not in Normalization Form KC." &
              " ARM 4.1/3: " &
              Compilation.Text_Name.To_UTF_16_Wide_String);
      end Not_In_NFKC_Warning;

      -------------------------
      -- Singe_File_Expected --
      -------------------------

      overriding procedure Singe_File_Expected (Self : access Handler)
      is
         pragma Unreferenced (Self);
      begin
         Implementation.Set_Status
           (Status    => Asis.Errors.Parameter_Error,
            Diagnosis => "Singe file name expected in Parameters");

         raise Asis.Exceptions.ASIS_Failed;
      end Singe_File_Expected;

      ------------------
      -- Syntax_Error --
      ------------------

      overriding procedure Syntax_Error
        (Self      : access Handler;
         File_Name : League.Strings.Universal_String)
      is
         pragma Unreferenced (Self);
      begin
         Implementation.Set_Status
           (Status    => Asis.Errors.Use_Error,
            Diagnosis => "Syntax error:" & File_Name.To_UTF_16_Wide_String);

         raise Asis.Exceptions.ASIS_Failed;
      end Syntax_Error;

   end Error_Handler;

   ------------
   -- Exists --
   ------------

   function Exists (The_Context : in Asis.Context) return Boolean is
   begin
      return Has_Associations (The_Context);
   end Exists;

   ----------------------
   -- Has_Associations --
   ----------------------

   function Has_Associations (The_Context : in Asis.Context)
      return Boolean is
   begin
      return Assigned (The_Context);
   end Has_Associations;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : in Asis.Context;
      Right : in Asis.Context) return Boolean is
   begin
      if not Assigned (Left) and not Assigned (Right) then
         return True;
      elsif Assigned (Left) and then
        Assigned (Right) and then
        Is_Identical (Left, Right)
      then
         return True;
      else
         return False;
      end if;
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : in Asis.Context;
      Right : in Asis.Context) return Boolean
   is
      use type Gela.Types.Context_Access;
   begin
      return Is_Open (Left) and Is_Open (Right) and
        Gela.Types.Context_Access (Left) = Gela.Types.Context_Access (Right);
   end Is_Identical;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (The_Context : in Asis.Context) return Boolean is
   begin
      return Assigned (The_Context) and then The_Context.Is_Open;
   end Is_Open;

   ----------
   -- Name --
   ----------

   function Name (The_Context : in Asis.Context) return Wide_String is
   begin
      if Assigned (The_Context) then
         return The_Context.Name.To_UTF_16_Wide_String;
      else
         return "";
      end if;
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open (The_Context : in out Asis.Context) is
      use Ada.Exceptions;
      use Asis.Exceptions;
      use Ada.Characters.Handling;
   begin
      if Is_Open (The_Context) then
         Implementation.Set_Status (
            Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has alredy been opened");

         raise ASIS_Inappropriate_Context;
      end if;

      if not Has_Associations (The_Context) then
         Implementation.Set_Status (
            Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has no association");

         raise ASIS_Inappropriate_Context;
      end if;

      The_Context.Open;

   exception
      when E : ASIS_Inappropriate_Context
        | ASIS_Failed =>
         Reraise_Occurrence (E);
      when E : others =>
         Implementation.Set_Status
           (Status    => Asis.Errors.Internal_Error,
            Diagnosis => "Asis.Ada_Environments.Open: "
            & To_Wide_String (Exception_Information (E)));
         raise ASIS_Failed;
   end Open;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (The_Context : in Asis.Context) return Wide_String is
   begin
      if Assigned (The_Context) then
         return The_Context.Parameters.To_UTF_16_Wide_String;
      else
         return "";
      end if;
   end Parameters;

end Asis.Ada_Environments;

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
