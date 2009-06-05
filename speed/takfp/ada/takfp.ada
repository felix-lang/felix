package Takfp_Pck is
   function Tak (X, Y, Z : Float) return Float;
end Takfp_Pck;

package body Takfp_Pck is

   function Tak (X, Y, Z : Float) return Float is
   begin
      if Y >= X then
         return Z;
      else
         return Tak
           (Tak (X - 1.0, Y, Z), Tak (Y - 1.0, Z, X), Tak (Z - 1.0, X, Y));
      end if;
   end Tak;

end Takfp_Pck;
---------------------------
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Takfp_Pck;         use Takfp_Pck;

procedure Takfp is
   N : Float;
begin
   N := Float'Value (Argument (1));
   Put (Tak (N * 3.0, N * 2.0, N * 1.0), 0, 1, 0);
   New_Line;
end Takfp;

