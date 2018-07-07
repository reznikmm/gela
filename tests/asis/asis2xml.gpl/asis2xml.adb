--  Copyright 2004-2014 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  Part of ASIS2XML.
--
--  Developed from Display_Source, which is distributed as a part of
--  the ASIS implementation for GNAT and is Copyright (c) 1995-1999,
--  Free Software Foundation, Inc.

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;

with Asis; use Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Exceptions;
with Asis.Implementation;

with DOM.Core.Nodes;

with XML_Support;
with String_Streams;

procedure ASIS2XML is

   function "+" (Item : String) return Wide_String
     renames Ada.Characters.Handling.To_Wide_String;

   function Ends_With (Full, Part : Wide_String) return Boolean;
   function Drop_Path (Text : String) return String;

   function Drop_Path (Text : String) return String is
   begin
      for J in 1 .. Ada.Command_Line.Argument_Count - 1 loop
         declare
            Arg : constant String := Ada.Command_Line.Argument (J);
            Index : Natural;
         begin
            if Arg (Arg'First .. Arg'First + 1) = "-I" then
               Index := Ada.Strings.Fixed.Index
                 (Text, Arg (Arg'First + 2 .. Arg'Last));

               if Index > 0 then
                  return Ada.Strings.Fixed.Replace_Slice
                    (Text, Index, Index + Arg'Length - 3, "PWD/");
               end if;
            end if;
         end;
      end loop;

      return Text;
   end Drop_Path;

   function Ends_With (Full, Part : Wide_String) return Boolean is
   begin
      return Full'Length >= Part'Length and then
        Full (Full'Last - Part'Length + 1 .. Full'Last) = Part;
   end Ends_With;

   procedure Usage;
   procedure Usage is
   begin
      Put_Line (Standard_Error,
                "usage: " &
                  Ada.Command_Line.Command_Name
                  & " [flags] associate_params source_file");
      Put_Line (Standard_Error, "flags:");
      Put_Line (Standard_Error, "-h          produce this output");
   end Usage;

   --  Some global variables.
   SS_Context : Asis.Context;

   XI : XML_Support.Info;
   Impl : DOM.Core.DOM_Implementation;
   Doc : DOM.Core.Document;
   Stream : aliased String_Streams.String_Stream;
begin

   Asis.Implementation.Initialize ("-asis05");

   begin
      loop
         case Ada.Command_Line.Argument_Count is
            when 3 .. 10 => exit;
            when 0 .. 2 =>
               Usage;
               return;
            when others => null;
               --  can't actually happen, raises Invalid_Switch
         end case;
      end loop;
   exception
      when Constraint_Error =>
         Usage;
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end;

   declare
      Params : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for J in 1 .. Ada.Command_Line.Argument_Count - 2 loop
         if J /= 1 then
            Ada.Strings.Unbounded.Append (Params, " ");
         end if;

         Ada.Strings.Unbounded.Append (Params, Ada.Command_Line.Argument (J));
      end loop;
      Asis.Ada_Environments.Associate
        (The_Context => SS_Context,
         Name => "SS_Context",
         Parameters  => +Ada.Strings.Unbounded.To_String (Params));
   end;

   Asis.Ada_Environments.Open (SS_Context);

   Doc  := DOM.Core.Create_Document (Impl);
   XML_Support.Initialize (XI, Doc);

   declare
      File_Name : constant Wide_String := +Ada.Command_Line.Argument
        (Ada.Command_Line.Argument_Count - 1);
      Next_Unit : Asis.Compilation_Unit;
      All_Units : constant Asis.Compilation_Unit_List
        := Asis.Compilation_Units.Compilation_Units (SS_Context);
   begin
      for I in All_Units'Range loop
         Next_Unit := All_Units (I);
         if Ends_With
           (Asis.Compilation_Units.Text_Name (Next_Unit), File_Name)
         then
            XML_Support.Add_Compilation_Unit (The_Unit => Next_Unit,
                                              To => XI);
         end if;
      end loop;
   end;

   XML_Support.Finalize (XI);

   Asis.Ada_Environments.Close (SS_Context);
   Asis.Ada_Environments.Dissociate (SS_Context);
   Asis.Implementation.Finalize ("");

   DOM.Core.Nodes.Write
     (Stream                => Stream'Access,
      N                     => Doc,
      Print_XML_Declaration => False,
      Pretty_Print          => True);

   declare
      use type Ada.Containers.Hash_Type;

      Text : constant String := Drop_Path (Stream.Get_Text);
      Found    : constant Ada.Containers.Hash_Type := Ada.Strings.Hash (Text);
      Expected : constant Ada.Containers.Hash_Type :=
        Ada.Containers.Hash_Type'Value
          (Ada.Command_Line.Argument (Ada.Command_Line.Argument_Count));

   begin
      if Found /= Expected then
         Put_Line (Ada.Containers.Hash_Type'Image (Found));
         Put_Line (Text);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end;

exception

   when Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit =>
      Put_Line (Standard_Error,
                "The input does not contain any Ada unit.");
      Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Asis.Exceptions.ASIS_Failed |
        Asis.Exceptions.ASIS_Inappropriate_Element |
        Asis.Exceptions.ASIS_Inappropriate_Context =>
      Put_Line (Standard_Error,
                Ada.Characters.Handling.To_String
                  (Asis.Implementation.Diagnosis));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Put_Line (Standard_Error,
                "exception received : " &
                 Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end ASIS2XML;
