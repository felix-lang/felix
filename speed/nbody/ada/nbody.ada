-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org
--
-- Contributed by Pascal Obry on 2005/03/21

with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package Nbody_Pck is

   type Real is Digits 15;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);

   Solar_Mass    : constant Real := 4.0 * Pi * Pi;
   Days_Per_Year : constant Real := 365.24;

   type Data is record
      X, Y, Z    : Real;
      Vx, Vy, Vz : Real;
      Mass       : Real;
   end record;

   type Body_Name is (Sun, Jupiter, Saturn, Uranus, Neptune);

   Bodies : array (Body_Name) of Data :=
              (Jupiter => (X    => 4.84143144246472090e+00,
                           Y    => -1.16032004402742839e+00,
                           Z    => -1.03622044471123109e-01,
                           Vx   => 1.66007664274403694e-03 * Days_Per_Year,
                           Vy   => 7.69901118419740425e-03 * Days_Per_Year,
                           Vz   => -6.90460016972063023e-05 * Days_Per_Year,
                           Mass => 9.54791938424326609e-04 * Solar_Mass),

               Saturn  => (X    => 8.34336671824457987e+00,
                           Y    => 4.12479856412430479e+00,
                           Z    => -4.03523417114321381e-01,
                           Vx   => -2.76742510726862411e-03 * Days_Per_Year,
                           Vy   => 4.99852801234917238e-03 * Days_Per_Year,
                           Vz   => 2.30417297573763929e-05 * Days_Per_Year,
                           Mass => 2.85885980666130812e-04 * Solar_Mass),

               Uranus  => (X    => 1.28943695621391310e+01,
                           y    => -1.51111514016986312e+01,
                           Z    => -2.23307578892655734e-01,
                           Vx   => 2.96460137564761618e-03 * Days_Per_Year,
                           Vy   => 2.37847173959480950e-03 * Days_Per_Year,
                           Vz   => -2.96589568540237556e-05 * Days_Per_Year,
                           Mass => 4.36624404335156298e-05 * Solar_Mass),

               Neptune => (X    => 1.53796971148509165e+01,
                           Y    => -2.59193146099879641e+01,
                           Z    => 1.79258772950371181e-01,
                           Vx   => 2.68067772490389322e-03 * Days_Per_Year,
                           Vy   => 1.62824170038242295e-03 * Days_Per_Year,
                           Vz   => -9.51592254519715870e-05 * Days_Per_Year,
                           Mass => 5.15138902046611451e-05 * Solar_Mass),

               Sun     => (X    => 0.0,
                           Y    => 0.0,
                           Z    => 0.0,
                           Vx   => 0.0,
                           Vy   => 0.0,
                           Vz   => 0.0,
                           Mass => Solar_Mass));

   procedure Offset_Momentum
     (Planet     : in out Data;
      Px, Py, Pz : in     Real);

   function Energy return Real;

   procedure Advance (Dt : in Real);

end Nbody_Pck;

-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org
--
-- Contributed by Pascal Obry on 2005/03/21

package body Nbody_Pck is

   procedure Offset_Momentum
     (Planet     : in out Data;
      Px, Py, Pz : in     Real) is
   begin
      Planet.Vx := -Px / Solar_Mass;
      Planet.Vy := -Py / Solar_Mass;
      Planet.Vz := -Pz / Solar_Mass;
   end Offset_Momentum;

   function Energy return Real is
      Dx, Dy, Dz, Distance : Real;
      E                    : Real := 0.0;
   begin
      for I in Bodies'Range loop
        E := E + 0.5 * Bodies (I).Mass
          * (Bodies (I).Vx * Bodies (I).Vx
               + Bodies (I).Vy * Bodies (I).Vy
               + Bodies (I).Vz * Bodies (I).Vz);

        if I /= Body_Name'Last then
           for J in Body_Name'Succ (I) .. Body_Name'Last loop
              Dx := Bodies (I).X - Bodies (J).X;
              Dy := Bodies (I).Y - Bodies (J).Y;
              Dz := Bodies (I).Z - Bodies (J).Z;

              Distance := Math.Sqrt (Dx * Dx + Dy * Dy + Dz * Dz);
              E := E - (Bodies (I).Mass * Bodies (J).Mass) / Distance;
           end loop;
        end if;
      end loop;
      return E;
   end Energy;

   procedure Advance (Dt : in Real) is
      Dx, Dy, Dz, Distance, Mag : Real;
   begin
      for I in Body_Name'Range loop
         if I /= Body_Name'Last then
            for J in Body_Name'Succ (I) .. Body_Name'Last loop
               Dx := Bodies (I).X - Bodies (J).X;
               Dy := Bodies (I).Y - Bodies (J).Y;
               Dz := Bodies (I).Z - Bodies (J).Z;

               Distance := Math.Sqrt (Dx * Dx + Dy * Dy + Dz * Dz);
               Mag := Dt / (Distance ** 3);

               Bodies (I).Vx := Bodies (I).Vx - Dx * Bodies (J).Mass * Mag;
               Bodies (I).Vy := Bodies (I).Vy - Dy * Bodies (J).Mass * Mag;
               Bodies (I).Vz := Bodies (I).Vz - Dz * Bodies (J).Mass * Mag;

               Bodies (J).Vx := Bodies (J).Vx + Dx * Bodies (I).Mass * Mag;
               Bodies (J).Vy := Bodies (J).Vy + Dy * Bodies (I).Mass * Mag;
               Bodies (J).Vz := Bodies (J).Vz + Dz * Bodies (I).Mass * Mag;
            end loop;
         end if;
      end loop;

      for I in Body_Name'Range loop
         Bodies (I).X := Bodies (I).X + Dt * Bodies (I).Vx;
         Bodies (I).Y := Bodies (I).Y + Dt * Bodies (I).Vy;
         Bodies (I).Z := Bodies (I).Z + Dt * Bodies (I).Vz;
      end loop;
   end Advance;

end Nbody_Pck;

-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org
--
-- Contributed by Pascal Obry on 2005/03/21

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Nbody_Pck;        use Nbody_Pck;

procedure Nbody is

   package RIO is new Float_Io (Real);

   procedure Put
     (Item : Real; Fore : Field := 0; Aft : Field := 9;
      Exp  : Field := 0) renames RIO.Put;

   N : constant Integer := 1000000 * Integer'Value (Argument (1));

   Px, Py, Pz : Real := 0.0;

begin
   for I in Body_Name'Range loop
      Px := Px + Bodies (I).Vx * Bodies (I).Mass;
      Py := Py + Bodies (I).Vy * Bodies (I).Mass;
      Pz := Pz + Bodies (I).Vz * Bodies (I).Mass;
   end loop;

   Offset_Momentum (Bodies (Sun), Px, Py, Pz);

   Put (Energy);
   New_Line;

   for K in 1 .. N loop
      Advance (0.01);
   end loop;

   Put (Energy);
   New_Line;
end Nbody;


