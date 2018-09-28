open Flx_exceptions

let terminate rrp exc =
  let return_parity = not rrp in
  if Printexc.backtrace_status () then begin
    print_endline (Printexc.get_backtrace ());
  end;
  match exc with
  | Exit n ->
    exit (if return_parity then n else (if n=0 then 1 else 0))

  | Flx_string.StringError s | Flx_utf.Utf8_to_Ucn_Error s ->
    flush stdout; print_endline "LEX ERROR";
    print_endline s;
    exit (if return_parity then 1 else 0)

  | TokenError s ->
    flush stdout; print_endline "TOKEN ERROR";
    print_endline s;
    exit (if return_parity then 1 else 0)

  | SyntaxError s ->
    flush stdout; print_endline "SYNTAX ERROR";
    print_endline s;
    exit (if return_parity then 1 else 0)

  | ParseError s ->
    flush stdout; print_endline "PARSE ERROR";
    print_endline s;
    exit (if return_parity then 1 else 0)

  | ClientError (sr,s) ->
    flush stdout; print_endline "CLIENT ERROR";
    print_endline s;
    print_endline ("In " ^ Flx_srcref.long_string_of_src sr);
    exit (if return_parity then 1 else 0)

  | ClientErrorn (srs,s) ->
    flush stdout; print_endline "CLIENT ERROR";
    print_endline s;
    List.iter (fun sr ->
      print_endline ("See: " ^ Flx_srcref.long_string_of_src sr)
    )
    srs
    ;
    exit (if return_parity then 1 else 0)

  | ClientError2 (sr,sr2,s) ->
    flush stdout; print_endline "CLIENT ERROR";
    print_endline s;
    print_endline ("In " ^ Flx_srcref.long_string_of_src sr);
    print_endline ("See also " ^ Flx_srcref.long_string_of_src sr2);
    exit (if return_parity then 1 else 0)

  | SystemError (sr,s) ->
    flush stdout; print_endline "FELIX COMPILER ERROR";
    print_endline ("In " ^ Flx_srcref.long_string_of_src sr);
    print_endline s;
    exit (if return_parity then 1 else 0)

  | Unresolved_return (sr,s) ->
    flush stdout; print_endline "UNRESOLVED RETURN ERROR";
    print_endline ("In " ^ Flx_srcref.long_string_of_src sr);
    print_endline s;
    exit (if return_parity then 1 else 0)

  | SimpleNameNotFound (sr,name,routine) ->
    flush stdout; print_endline ("SIMPLE NAME "^name^" NOT FOUND ERROR");
    print_endline ("In " ^ Flx_srcref.long_string_of_src sr);
    print_endline ("Routine: " ^ routine);
    exit (if return_parity then 1 else 0)

  | FunctionNameNotFound (sr,name,routine, args) ->
    flush stdout; print_endline ("FUNCTION NAME "^name^" NOT FOUND ERROR");
    print_endline ("In " ^ Flx_srcref.long_string_of_src sr);
    print_endline ("Routine: " ^ routine);
    print_endline ("Argument types: " ^ String.concat ", " args);
    exit (if return_parity then 1 else 0)

  | Failure s ->
    flush stdout; print_endline "SYSTEM FAILURE";
    print_endline s;
    exit (if return_parity then 1 else 0)

  | x ->
    print_endline ("Fatal error: exception " ^ (Printexc.to_string x));
    exit (if return_parity then 1 else 0)

