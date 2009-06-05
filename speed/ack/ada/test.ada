with Ada.Text_Io; use Ada.Text_Io;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed;
with Ack_F;

procedure Ack is
  Num : Natural;
  function L_Trim(Source : String; Side : Ada.Strings.Trim_End := Ada.Strings.Left) return String renames Ada.Strings.Fixed.Trim;
begin
  if Argument_Count = 1 then
    Num := Natural'Value(Argument(1));
  else
    Num := 1;
  end if;

  Put("Ack(3,");
  Put(L_Trim ( Natural'Image (Num)));
  Put("):");
  Put(Natural'Image (Ack_F (3, Num)));
end Ack;

----------------------------------------------------------------------

function Ack_F (M, N : Natural) return Natural;

----------------------------------------------------------------------

function Ack_F (M, N : Natural) return Natural is
pragma Suppress(All_Checks);
begin
   if M = 0 then
      return N + 1;
   elsif N = 0 then
      return Ack_F (M - 1, 1);
   else
      return Ack_F (M - 1, Ack_F (M, N - 1));
   end if;
end Ack_F;

