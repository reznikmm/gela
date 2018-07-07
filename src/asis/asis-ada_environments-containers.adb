------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  Purpose:
--  Trivial implementation of ASIS Ada_Environments.Containers

with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Ada_Environments.Containers.Internals;

with Gela.Compilation_Unit_Sets;

package body Asis.Ada_Environments.Containers is

   -----------------------------
   -- Compilation_Unit_Bodies --
   -----------------------------

   function Compilation_Unit_Bodies
     (The_Container : in Container)
      return Asis.Compilation_Unit_List is
   begin
      if Is_Open (The_Container.Context) then
         declare
            Set : constant Gela.Compilation_Unit_Sets.
              Compilation_Unit_Set_Access :=
                The_Container.List.Compilation_Unit_Bodies;
         begin
            return Internals.To_List (Set);
         end;
      else
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context is not opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;
   end Compilation_Unit_Bodies;

   -----------------------
   -- Compilation_Units --
   -----------------------

   function Compilation_Units
     (The_Container : in Container)
      return Asis.Compilation_Unit_List is
   begin
      return Library_Unit_Declarations (The_Container) &
        Compilation_Unit_Bodies (The_Container);
   end Compilation_Units;

   -------------------------
   -- Defining_Containers --
   -------------------------

   function Defining_Containers
     (The_Context : in Asis.Context)
      return Container_List
   is
      Containers : constant Gela.Unit_Containers.Unit_Container_List :=
        The_Context.Implementation.Unit_Containers;
      Index  : Natural := Containers'First;
      Result : Container_List (1 .. Asis.ASIS_Natural (Containers'Length));
   begin
      for J in Result'Range loop
         Result (J) :=
           (Context => The_Context,
            List    => Containers (Index));

         Index := Index + 1;
      end loop;

      return Result;
   end Defining_Containers;

   -----------------------
   -- Enclosing_Context --
   -----------------------

   function Enclosing_Context
     (The_Container : in Container) return Asis.Context is
   begin
      return The_Container.Context;
   end Enclosing_Context;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : in Container; Right : in Container) return Boolean
   is
      Left_Units  : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
      Right_Units : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
   begin
      Left_Units := Left.List.Compilation_Unit_Bodies;
      Right_Units := Right.List.Compilation_Unit_Bodies;

      if not Equal (Left_Units,
                    Right_Units,
                    Left.Context.Implementation.Symbols,
                    Right.Context.Implementation.Symbols)
      then
         return False;
      end if;

      Left_Units := Left.List.Library_Unit_Declarations;
      Right_Units := Right.List.Library_Unit_Declarations;

      return Equal (Left_Units,
                    Right_Units,
                    Left.Context.Implementation.Symbols,
                    Right.Context.Implementation.Symbols);
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : in Container; Right : in Container) return Boolean
   is
      use type Gela.Unit_Containers.Unit_Container_Access;
   begin
      return Is_Identical (Left.Context, Right.Context) and
        Left.List = Right.List;
   end Is_Identical;

   -------------------------------
   -- Library_Unit_Declarations --
   -------------------------------

   function Library_Unit_Declarations
     (The_Container : in Container) return Asis.Compilation_Unit_List is
   begin
      if Is_Open (The_Container.Context) then
         declare
            Set : constant Gela.Compilation_Unit_Sets.
              Compilation_Unit_Set_Access :=
                The_Container.List.Library_Unit_Declarations;
         begin
            return Internals.To_List (Set);
         end;
      else
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Context is not opened");

         raise Exceptions.ASIS_Inappropriate_Context;
      end if;
   end Library_Unit_Declarations;

   ----------
   -- Name --
   ----------

   function Name (The_Container : in Container) return Wide_String is
   begin
      return The_Container.List.Name.To_UTF_16_Wide_String;
   end Name;

end Asis.Ada_Environments.Containers;



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
