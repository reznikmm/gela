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

with League.Application;
with League.Strings;
with League.String_Vectors;

with Gela.Context_Factories;
with Gela.Contexts;
with Gela.Lexical_Types;

package body Asis.Ada_Environments is

   function Assigned (Self : Asis.Context) return Boolean;

   --------------
   -- Assigned --
   --------------

   function Assigned (Self : Asis.Context) return Boolean is
      use type Gela.Contexts.Context_Access;
   begin
      return Self.Implementation /= null;
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
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context has alredy been opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;

      The_Context :=
        (Implementation => null,
         Name           => League.Strings.From_UTF_16_Wide_String (Name),
         Parameters     => League.Strings.From_UTF_16_Wide_String (Parameters),
         Associated     => True);
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

      --  Free (The_Context.Implementation);  TODO
      The_Context.Implementation := null;
   end Close;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (The_Context : in Asis.Context) return Wide_String is
   begin
      return "Name: " & Name (The_Context) &
        " Parameters:" & Parameters (The_Context);
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
      if Is_Open (The_Context) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context is opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;

      The_Context := Nil_Context;
   end Dissociate;

   -----------
   -- Equal --
   -----------

   function Equal
     (A, B      : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
      A_Symbols : Gela.Symbol_Sets.Symbol_Set_Access;
      B_Symbols : Gela.Symbol_Sets.Symbol_Set_Access)
      return Boolean is
   begin
      if A.Length /= B.Length then
         return False;
      end if;

      declare
         use type League.Strings.Universal_String;
         use type Gela.Lexical_Types.Symbol;

         A_Unit : Gela.Compilation_Units.Compilation_Unit_Access;
         B_Unit : Gela.Compilation_Units.Compilation_Unit_Access;
         A_Cursor : Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class
           := A.First;
         B_Cursor : Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class
           := B.First;
      begin
         while A_Cursor.Has_Element loop
            A_Unit := A_Cursor.Element;
            B_Unit := B_Cursor.Element;

            if A_Symbols.Folded (A_Unit.Name) /=
              B_Symbols.Folded (B_Unit.Name)
            then
               return False;
            end if;

            A_Cursor.Next;
            B_Cursor.Next;
         end loop;
      end;

      return True;
   end Equal;

   ------------
   -- Exists --
   ------------

   function Exists (The_Context : in Asis.Context) return Boolean is
   begin
      return Is_Open (The_Context);
   end Exists;

   ----------------------
   -- Has_Associations --
   ----------------------

   function Has_Associations (The_Context : in Asis.Context) return Boolean is
   begin
      return The_Context.Associated;
   end Has_Associations;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : in Asis.Context;
      Right : in Asis.Context) return Boolean
   is
      Left_Units  : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
      Right_Units : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      if Is_Open (Left) and Is_Open (Right) then
         Left_Units := Left.Implementation.Library_Unit_Declarations;
         Right_Units := Left.Implementation.Library_Unit_Declarations;

         if not Equal (Left_Units,
                       Right_Units,
                       Left.Implementation.Symbols,
                       Right.Implementation.Symbols)
         then
            return False;
         end if;

         Left_Units := Left.Implementation.Compilation_Unit_Bodies;
         Right_Units := Left.Implementation.Compilation_Unit_Bodies;

         return Equal (Left_Units,
                       Right_Units,
                       Left.Implementation.Symbols,
                       Right.Implementation.Symbols);
      else
         return Name (Left) = Name (Right) and
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
      use type Gela.Contexts.Context_Access;
   begin
      return Left.Implementation = Right.Implementation;
   end Is_Identical;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (The_Context : in Asis.Context) return Boolean is
      use type Gela.Contexts.Context_Access;
   begin
      return The_Context.Implementation /= null;
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

      declare
         Gela_Include_Path : constant League.Strings.Universal_String :=
           League.Strings.To_Universal_String ("GELA_INCLUDE_PATH");
         Env  : constant League.Strings.Universal_String :=
           League.Application.Environment.Value (Gela_Include_Path);
         Args : constant League.String_Vectors.Universal_String_Vector :=
           The_Context.Parameters.Split (' ');
      begin
         The_Context.Implementation :=
           Gela.Context_Factories.Create_Context (Args, Env);
      exception
         when E : others =>
            Implementation.Set_Status
              (Status    => Asis.Errors.Internal_Error,
               Diagnosis => "Asis.Ada_Environments.Open: "
               & To_Wide_String (Exception_Information (E)));
            raise ASIS_Failed;
      end;
   end Open;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (The_Context : in Asis.Context) return Wide_String is
   begin
      return The_Context.Parameters.To_UTF_16_Wide_String;
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
