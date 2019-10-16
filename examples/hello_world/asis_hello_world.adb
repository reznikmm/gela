with Asis;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Ada_Environments;
with Asis.Compilation_Units;

with Ada.Exceptions;
with Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

procedure Main is

   procedure Help is
   begin
      Ada.Wide_Text_IO.Put_Line ("prog [-I/path/to/adalib] -I/path/to/sources/ [--debug=<Prop_List>] unit.adb");
      Ada.Wide_Text_IO.Put_Line ("   <Prop_List> ::= <prop>[,<Prop_List>]");
      Ada.Wide_Text_IO.Put_Line ("   <Prop>      ::= UP | DOWN | EN_IN | ENV_OUT | FULL_NAME");
      Ada.Wide_Text_IO.Put_Line ("");
      Ada.Wide_Text_IO.Put_Line ("Note: You should provide a path to standard library files through -I");      
      Ada.Wide_Text_IO.Put_Line ("or through GELA_INCLUDE_PATH environment variable.");
   end Help;
   
   Invalid_Number_Of_Arguments : exception;
   
   My_Context_Name           : constant Wide_String           := Asis.Ada_Environments.Default_Name;
   Initialization_Parameters : constant Wide_String           := "";
   Finalization_Parameters   : constant Wide_String           := "";
   My_Context_Parameters     :          Unbounded_Wide_String := Null_Unbounded_Wide_String;
   My_Context                :          Asis.Context;

begin
   if Argument_Count not in 2 .. 4 then
      if Argument_Count = 0 then
         Help;
      else
         raise Invalid_Number_Of_Arguments;
      end if;
   end if;

   Append (My_Context_Parameters, To_Wide_String (Argument (1)));
   for I in 2 .. Argument_Count loop
      Append (My_Context_Parameters, " " & To_Wide_String (Argument (I)));
   end loop;

   Asis.Implementation.Initialize     (Initialization_Parameters);
   Asis.Ada_Environments.Associate
     (The_Context => My_Context,
      Name        => My_Context_Name,
      Parameters  => To_Wide_String (My_Context_Parameters));
   Asis.Ada_Environments.Open         (My_Context);
   declare
      use Asis.Compilation_Units;
      Units : Asis.Compilation_Unit_List := Compilation_Units (My_Context);
   begin
      for J in Units'Range loop
         Ada.Wide_Text_IO.Put_Line
           (Text_Name (Units (J)) & " => " & Unit_Full_Name (Units (J)));
      end loop;
   end;

   Asis.Ada_Environments.Close        (My_Context);
   Asis.Ada_Environments.Dissociate   (My_Context);
   Asis.Implementation.Finalize       (Finalization_Parameters);

   Set_Exit_Status (Success);

exception
   when E : Asis.Exceptions.ASIS_Inappropriate_Context          |
            Asis.Exceptions.ASIS_Inappropriate_Container        |
            Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
            Asis.Exceptions.ASIS_Inappropriate_Element          |
            Asis.Exceptions.ASIS_Inappropriate_Line             |
            Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
            Asis.Exceptions.ASIS_Failed                         =>

      Ada.Wide_Text_IO.Put_Line
        ("ASIS exception (" &
         To_Wide_String (Ada.Exceptions.Exception_Name (E)) &
         ") is raised");

      Ada.Wide_Text_IO.Put_Line
        ("ASIS Error Status is " &
         Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status));

      Ada.Wide_Text_IO.Put_Line ("ASIS Diagnosis is ");
      Ada.Wide_Text_IO.Put_Line (Asis.Implementation.Diagnosis);

      Asis.Implementation.Set_Status;
   when Invalid_Number_Of_Arguments =>
      Ada.Wide_Text_IO.Put_Line (">>> Invalid number of arguments");
      Help;
end Main;
