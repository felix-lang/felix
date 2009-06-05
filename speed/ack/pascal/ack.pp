program ack;

function ack(x:longint; y:longint):longint;
  begin
    if x = 0 then ack :=  y + 1
    else if y = 0 then ack := ack(x-1,1)
    else ack := ack(x-1, ack(x, y-1))
  end
;

var
  n,w: integer; s: string;
begin
  s := paramstr(1); val(s, n, w); if w <> 0 then n := 1;
  write('Ack(3,'); write(n); write(') : '); writeln(ack(3,n));
end.

