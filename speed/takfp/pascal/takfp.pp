program takfp;

function tak(x,y,z:real):real;
  begin
    if y >= x then tak := z
    else tak := tak (tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y))
  end
;

var
  n,w: integer; s: string;
begin
  s := paramstr(1); val(s, n, w); if w <> 0 then n := 1;
  writeln(tak(3*n,2*n,n):0:1);
end.



