------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-ada_environments.adb 2455 2006-06-24 19:22:06Z maxr $
--  Purpose:
--  Implementation of Asis.Ada_Environments

with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

with Ada.Strings.Wide_Unbounded;

with Ada.Exceptions;
with Ada.Characters.Handling;

with Asis.Gela.Contexts;
with Asis.Gela.Current_State;

package body Asis.Ada_Environments is
   use Asis.Gela.Contexts;

   Contexts : Asis.Gela.Contexts.Context_List renames
     Asis.Gela.Current_State.Contexts;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (The_Context : in out Asis.Context;
      Name        : in     Wide_String;
      Parameters  : in     Wide_String := Default_Parameters)
   is
   begin
      if not Implementation.Is_Initialized or Implementation.Is_Finalized then
         Implementation.Set_Status
           (Status    => Asis.Errors.Initialization_Error,
            Diagnosis => "ASIS is not initialized");

         raise Exceptions.ASIS_Failed;
      end if;

      if Is_Open (The_Context) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has alredy been opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;

      if The_Context.Index = 0  then
         for J in Contexts'Range loop
            if not Is_Associated (Contexts (J)) then
               The_Context.Index := J;
               exit;
            end if;
         end loop;
      end if;

      if The_Context.Index = 0 then
         Implementation.Set_Status
           (Status    => Asis.Errors.Capacity_Error,
            Diagnosis => "Too many open contexts");

         raise Exceptions.ASIS_Failed;
      end if;

      declare
         C : Asis.Gela.Contexts.Context_Node renames
           Contexts (The_Context.Index);
      begin
         Asis.Gela.Contexts.Parse_Parameters
           (C, The_Context, Name, Parameters);
      end;
   end Associate;

   -----------
   -- Close --
   -----------

   procedure Close (The_Context : in out Asis.Context) is
   begin
      if not Is_Open (The_Context) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context is not opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;

      Close (Contexts (The_Context.Index));
   end Close;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (The_Context : in Asis.Context) return Wide_String is
   begin
      return Context_Index'Wide_Image (The_Context.Index);
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

   procedure Dissociate (The_Context : in out Asis.Context) is
   begin
      if not Implementation.Is_Initialized or Implementation.Is_Finalized then
         Implementation.Set_Status
           (Status    => Asis.Errors.Initialization_Error,
            Diagnosis => "ASIS is not initialized");

         raise Exceptions.ASIS_Failed;
      end if;

      if Is_Open (The_Context) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has alredy been opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;

      if The_Context.Index > 0 then
         Dissociate (Contexts (The_Context.Index));
         The_Context.Index := 0;
      end if;
   end Dissociate;

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

   function Has_Associations (The_Context : in Asis.Context) return Boolean is
   begin
      return The_Context.Index > 0;
   end Has_Associations;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : in Asis.Context;
      Right : in Asis.Context) return Boolean
   is
   begin
      if Is_Open (Left) and Is_Open (Right) then
         return Left.Index = Right.Index;
      else
         return Name (Left) = Name (Right) and then
           Parameters (Left) = Parameters (Right);
      end if;
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : in Asis.Context;
      Right : in Asis.Context) return Boolean
   is
   begin
      return Is_Open (Left) and Is_Open (Right) and Left.Index = Right.Index;
   end Is_Identical;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (The_Context : in Asis.Context) return Boolean is
   begin
      return The_Context.Index > 0 and then
        Is_Open (Contexts (The_Context.Index));
   end Is_Open;

   ----------
   -- Name --
   ----------

   function Name (The_Context : in Asis.Context) return Wide_String is
   begin
      if Has_Associations (The_Context) then
         return Gela.Contexts.Name (Contexts (The_Context.Index));
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
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has alredy been opened");

         raise ASIS_Inappropriate_Context;
      end if;

      if not Has_Associations (The_Context) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has no association");

         raise ASIS_Inappropriate_Context;
      end if;

      Asis.Gela.Contexts.Open (Contexts (The_Context.Index));

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
      if Has_Associations (The_Context) then
         return Gela.Contexts.Parameters (Contexts (The_Context.Index));
      else
         return "";
      end if;
   end Parameters;

end Asis.Ada_Environments;



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
