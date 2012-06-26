with Ada.Strings.Wide_Unbounded.Wide_Hash;

with Asis.Declarations;
with Asis.Elements;
with Asis.Expressions;

with XASIS.Utils;

package body Traversing_Actions is

   procedure Print_Corresponding_Defining_Name
     (State   : in out Traversal_State;
      Element : Asis.Element);

   New_Line : constant Wide_Character := Wide_Character'Val (10);
   
   --------------
   -- Get_Text --
   --------------

   function Get_Text (State : Traversal_State) return Wide_String is
   begin
      return Ada.Strings.Wide_Unbounded.To_Wide_String (State.Text);
   end Get_Text;
   
   -------------------
   -- Get_Text_Hash --
   -------------------

   function Get_Text_Hash
     (State : Traversal_State)
      return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Wide_Unbounded.Wide_Hash (State.Text);
   end Get_Text_Hash;
   
   -----------------
   -- Post_Action --
   -----------------

   procedure Post_Action
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State)
   is
      pragma Unreferenced (Control, Element);
   begin
      State.Level := State.Level - 1;
   end Post_Action;

   ----------------
   -- Pre_Action --
   ----------------

   procedure Pre_Action
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State)
   is
      use Ada.Strings.Wide_Unbounded;
      pragma Unreferenced (Control);
   begin
      Append (State.Text, State.Level * 2 * ' ');
      Append (State.Text, XASIS.Utils.Debug_Image (Element));
      Print_Corresponding_Defining_Name (State, Element);
      Append (State.Text, New_Line);
      State.Level := State.Level + 1;
   end Pre_Action;

   ---------------------------------------
   -- Print_Corresponding_Defining_Name --
   ---------------------------------------

   procedure Print_Corresponding_Defining_Name
     (State   : in out Traversal_State;
      Element : Asis.Element)
   is
      Defining_Name : Asis.Defining_Name;
   begin
      case Asis.Elements.Expression_Kind (Element) is
         when Asis.An_Identifier |
              Asis.An_Operator_Symbol |
              Asis.A_Character_Literal |
              Asis.An_Enumeration_Literal
            =>
            Defining_Name :=
              Asis.Expressions.Corresponding_Name_Definition (Element);
         when others =>
            return;
      end case;
      
      if Asis.Elements.Is_Nil (Defining_Name) then
         return;
      end if;
      
      declare
         use Ada.Strings.Wide_Unbounded;
         Image : constant Wide_String :=
           Asis.Declarations.Defining_Name_Image (Defining_Name);
      begin
         if Image /= "" then
            Append (State.Text, New_Line);
            Append (State.Text, State.Level * 2 * ' ');
            Append (State.Text, "Corresponding_Defining_Name=");
            Append (State.Text, XASIS.Utils.Debug_Image (Defining_Name));
         end if;
      end;
   end Print_Corresponding_Defining_Name;
   
end Traversing_Actions;


------------------------------------------------------------------------------
--  Copyright (c) 2006-2012, Maxim Reznik
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
