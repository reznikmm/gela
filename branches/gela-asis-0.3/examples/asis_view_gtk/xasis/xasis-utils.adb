with Ada.Characters.Handling;

with Asis.Text;
with Asis.Elements;
with Asis.Iterator;
with Asis.Statements;
with Asis.Definitions;
with Asis.Expressions;
with Asis.Declarations;
with Asis.Compilation_Units;

with Ada.Wide_Text_IO;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;

package body XASIS.Utils is

   use Asis;


   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (Element : in Asis.Element) return Asis.Program_Text is
      use Asis;
      use Asis.Elements;
      use Asis.Expressions;
      use Asis.Declarations;

      function Image return Wide_String is
      begin
         case Element_Kind (Element) is
            when A_Defining_Name =>
               return Defining_Name_Image (Element);
            when An_Expression =>
               case Expression_Kind (Element) is
                  when An_Integer_Literal
                    | A_Real_Literal
                    | A_String_Literal =>
                     return Value_Image (Element);
                  when An_Identifier
                    | An_Operator_Symbol
                    | A_Character_Literal
                    | An_Enumeration_Literal =>
                     return Name_Image (Element);
                  when others =>
                     return "";
               end case;
            when A_Declaration =>
               declare
                  List : constant Asis.Element_List := Names (Element);
               begin
                  if List'Length > 0 then
                     return Defining_Name_Image (List (List'First));
                  else
                     return "";
                  end if;
               end;
            when others =>
               return "";
         end case;
      end Image;

      function Kind_Image return Wide_String is
      begin
         case Elements.Element_Kind (Element) is
            when A_Pragma =>
               return "Pragma_Kind:"
                  & Pragma_Kinds'Wide_Image (Elements.Pragma_Kind (Element));

            when A_Defining_Name =>
               return "Defining_Name_Kind:"
                 & Defining_Name_Kinds'Wide_Image
                 (Elements.Defining_Name_Kind (Element));

            when A_Declaration =>
               return "Declaration_Kind:"
                 & Declaration_Kinds'Wide_Image
                 (Elements.Declaration_Kind (Element));

            when A_Definition =>
               case Elements.Definition_Kind (Element) is
                  when A_Type_Definition =>
                     return "Type_Kind:"
                       & Type_Kinds'Wide_Image
                       (Elements.Type_Kind (Element));

                  when A_Constraint =>
                     return "Constraint_Kind:"
                        & Constraint_Kinds'Wide_Image
                        (Elements.Constraint_Kind (Element));

                  when A_Discrete_Subtype_Definition =>
                     return "S-Discrete_Range_Kind:"
                        & Discrete_Range_Kinds'Wide_Image
                        (Elements.Discrete_Range_Kind (Element));

                  when A_Discrete_Range =>
                     return "Discrete_Range_Kind:"
                        & Discrete_Range_Kinds'Wide_Image
                        (Elements.Discrete_Range_Kind (Element));

                  when A_Formal_Type_Definition =>
                     return "Formal_Type_Kind:"
                        & Formal_Type_Kinds'Wide_Image
                        (Elements.Formal_Type_Kind (Element));

                  when others =>
                     return "Definition_Kind:"
                        & Definition_Kinds'Wide_Image
                        (Elements.Definition_Kind (Element));
               end case;

            when An_Expression =>
               return "Expression_Kind:"
                  & Expression_Kinds'Wide_Image
                  (Elements.Expression_Kind (Element));

            when An_Association =>
               return "Association_Kind:"
                  & Association_Kinds'Wide_Image
                  (Elements.Association_Kind (Element));

            when A_Statement =>
               return "Statement_Kind:"
                  & Statement_Kinds'Wide_Image
                  (Elements.Statement_Kind (Element));

            when A_Path =>
               return "Path_Kind:"
                  & Path_Kinds'Wide_Image
                  (Elements.Path_Kind (Element));

            when A_Clause =>
               case Elements.Clause_Kind (Element) is
                  when A_Representation_Clause =>
                     return "Representation_Clause_Kind:"
                       & Representation_Clause_Kinds'Wide_Image
                       (Elements.Representation_Clause_Kind (Element));

                  when others =>
                     return "Clause_Kind:"
                       & Clause_Kinds'Wide_Image
                       (Elements.Clause_Kind (Element));
               end case;

            when others =>
               return "Element_Kind:"
                 & Element_Kinds'Wide_Image (Elements.Element_Kind (Element));
         end case;
      end Kind_Image;

      function Span_Image return Wide_String is
         use Asis.Text;
         function Img (X : Asis.ASIS_Natural) return Wide_String is
            Image   : constant Wide_String :=
              Asis.ASIS_Natural'Wide_Image (X);
         begin
            return Image (2 .. Image'Last);
         end Img;

         X : constant Span := Element_Span (Element);

      begin
         return " [" & Img (X.First_Line)
           & ":" & Img (X.First_Column)
           & ".." & Img (X.Last_Line)
           & ":" & Img (X.Last_Column)
           & "]";
      end Span_Image;

      function File_Name return Wide_String is
         use Ada.Strings;
         Name  : constant Wide_String :=
           Compilation_Units.Text_Name (Enclosing_Compilation_Unit (Element));
         Slash : Natural := Wide_Fixed.Index (Name, "/", Backward);
      begin
         if Slash = 0 then
            return Name;
         else
            return Name (Slash + 1 .. Name'Last);
         end if;
      end  File_Name;

      function Explicit_Parent return Asis.Element is
         Result : Asis.Element := Enclosing_Element (Element);
      begin
         while Is_Part_Of_Implicit (Result)
           or Is_Part_Of_Inherited (Result)
           or Is_Part_Of_Instance (Result)
         loop
            Result := Enclosing_Element (Result);
         end loop;

         return Result;
      end Explicit_Parent;

      function Generic_Element_Image return Wide_String is
      begin
         if Element_Kind (Element) = A_Defining_Name then
            return "of "
              & Debug_Image (Corresponding_Generic_Element (Element));
         else
            return "";
         end if;
      end Generic_Element_Image;

   begin
      if not Is_Nil (Element) then
         if Is_Part_Of_Instance (Element) then
            return Kind_Image
              & " "
              & Image
              & " instance "
              & Generic_Element_Image
              & " in "
              & Debug_Image (Explicit_Parent);
         elsif Is_Part_Of_Inherited (Element) then
            return Kind_Image
              & " "
              & Image
              & " inherited in "
              & Debug_Image (Explicit_Parent);
         elsif Is_Part_Of_Implicit (Element) then
            return Kind_Image
              & " "
              & Image
              & " implicit of "
              & Debug_Image (Explicit_Parent);
         else
            return Kind_Image
              & " "
              & Image
              & " at "
              & File_Name
              & Span_Image;
         end if;
      else
         return "[Not_An_Element]";
      end if;
   end Debug_Image;

end XASIS.Utils;



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
