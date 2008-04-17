(*
 *
 * Copyright (c) 2001-2003,
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 *  Ben Liblit          <liblit@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)


module E = Errormsg

let setDebugFlag v name = 
  E.debugFlag := v;
  if v then Pretty.flushOften := true

type outfile = 
    { fname: string;
      fchan: out_channel } 

      (* Processign of output file arguments *)
let openFile (what: string) (takeit: outfile -> unit) (fl: string) = 
  if !E.verboseFlag then
    ignore (Printf.printf "Setting %s to %s\n" what fl);
  (try takeit { fname = fl;
                fchan = open_out fl }
  with _ ->
    raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ fl)))


let fileNames : string list ref = ref []
let recordFile fname = 
  fileNames := fname :: (!fileNames) 

                         (* Parsing of files with additional names *)
let parseExtraFile (s: string) = 
  try
    let sfile = open_in s in
    while true do
      let line = try input_line sfile with e -> (close_in sfile; raise e) in
      let linelen = String.length line in
      let rec scan (pos: int) (* next char to look at *)
          (start: int) : unit (* start of the word, 
                                 or -1 if none *) =
        if pos >= linelen then 
          if start >= 0 then 
            recordFile (String.sub line start (pos - start))
          else 
            () (* Just move on to the next line *)
        else
          let c = String.get line pos in
          match c with 
            ' ' | '\n' | '\r' | '\t' -> 
              (* whitespace *)
              if start >= 0 then begin
                recordFile (String.sub line start (pos - start));
              end;
              scan (pos + 1) (-1)
                
          | _ -> (* non-whitespace *)
              if start >= 0 then 
                scan (pos + 1) start 
              else
                scan (pos + 1) pos
      in
        scan 0 (-1)
    done
  with Sys_error _ -> E.s (E.error "Cannot find extra file: %s\n" s)
  |  End_of_file -> () 


let options : (string * Arg.spec * string) list =
  let is_default = function
      true -> " (default)"
    | false -> "" in
  [
    (* General Options *)
    "", Arg.Unit (fun () -> ()), " \n\t\tGeneral Options\n";

    "--version",
    Arg.Unit (fun _ ->
                print_endline ("CIL version " ^ Cil.cilVersion ^
                                 "\nMore information at http://cil.sourceforge.net/\n");
                exit 0),
    " Output version information and exit";

    "--verbose",
    Arg.Set E.verboseFlag,
    (" Print lots of random stuff; this is passed on from cilly" ^
       is_default !E.verboseFlag);

    "--noverbose",
    Arg.Clear E.verboseFlag,
    (" Undo effect of verbose flag" ^ is_default (not !E.verboseFlag));

    "--warnall",
    Arg.Set E.warnFlag,
    (" Show optional warnings" ^ is_default !E.warnFlag);

    "--nowarnall",
    Arg.Clear E.warnFlag,
    (" Disable optional warnings" ^ is_default (not !E.warnFlag));

    "--noTruncateWarning", 
    Arg.Clear Cil.warnTruncate,
    " Suppress warning about truncating integer constants";

    "--debug",
    Arg.String (setDebugFlag true),
    "<xxx> Turn on debugging flag xxx";

    "--nodebug",
    Arg.String (setDebugFlag false),
    "<xxx> Turn off debugging flag xxx";

    "--flush",
    Arg.Set Pretty.flushOften,
    (" Flush the output streams often; aids debugging" ^
       is_default !Pretty.flushOften);

    "--noflush",
    Arg.Clear Pretty.flushOften,
    (" Only flush output streams when inevitable" ^
       is_default (not !Pretty.flushOften));

    "--check",
    Arg.Set Cilutil.doCheck,
    (" Run a consistency check over the CIL after every operation" ^
       is_default !Cilutil.doCheck);

    "--nocheck",
    Arg.Clear Cilutil.doCheck,
    (" Turn off consistency checking of CIL" ^
       is_default (not !Cilutil.doCheck));

    "--strictcheck", Arg.Unit (fun _ -> Cilutil.doCheck := true;
                                        Cilutil.strictChecking := true),
                     " Same as --check, but treats problems as errors not warnings.";
    "", Arg.Unit (fun _ -> ()), "";

    "--noPrintLn",
    Arg.Unit (fun _ ->
                Cil.lineDirectiveStyle := None;
                Cprint.printLn := false),
    " Do not output #line directives in the output";

    "--commPrintLn",
    Arg.Unit (fun _ ->
                Cil.lineDirectiveStyle := Some Cil.LineComment;
                Cprint.printLnComment := true),
    " Print #line directives in the output, but put them in comments";

    "--commPrintLnSparse", 
    Arg.Unit (fun _ ->
                Cil.lineDirectiveStyle := Some Cil.LineCommentSparse;
                Cprint.printLnComment := true),
    " Print commented #line directives in the output only when\n\t\t\t\tthe line number changes.";

    "--stats",
    Arg.Set Cilutil.printStats,
    (" Print statistics about running times and memory usage" ^
       is_default !Cilutil.printStats);

    "--nostats",
    Arg.Clear Cilutil.printStats,
    (" Do not print statistics" ^
       is_default (not !Cilutil.printStats));

    "--log",
    Arg.String (openFile "log" (fun oc -> E.logChannel := oc.fchan)),
    "<filename> Set the name of the log file; by default use stderr";

    "--MSVC",
    Arg.Unit (fun _ ->
                Cil.msvcMode := true;
                Frontc.setMSVCMode ();
                if not Machdep.hasMSVC then
                  ignore (E.warn "Will work in MSVC mode but will be using machine-dependent parameters for GCC since you do not have the MSVC compiler installed\n")),
    " Enable MSVC compatibility; default is GNU";

    "--testcil",
    Arg.String (fun s -> Cilutil.testcil := s),
    "<compiler> Test CIL using the given compiler";

    "--ignore-merge-conflicts",
    Arg.Set Mergecil.ignore_merge_conflicts,
    (" Ignore merging conflicts" ^
       is_default !Mergecil.ignore_merge_conflicts);

(* Little-used: *)
(*     "--noignore-merge-conflicts", *)
(*     Arg.Clear Mergecil.ignore_merge_conflicts, *)
(*     (" Do not ignore merging conflicts" ^ *)
(*        is_default (not !Mergecil.ignore_merge_conflicts)); *)

    "--sliceGlobal",
    Arg.Set Cilutil.sliceGlobal,
    " Output is the slice of #pragma cilnoremove(sym) symbols";

    (* sm: some more debugging options *)
    "--tr",
    Arg.String Trace.traceAddMulti,
    "<sys> Subsystem to show debug printfs for";

    "--extrafiles",
    Arg.String parseExtraFile,
    "<filename> File that contains a list of additional files to process,\n\t\t\t\tseparated by newlines";

    (* Lowering Options *)
    "", Arg.Unit (fun () -> ()), " \n\t\tLowering Options\n";

    "--lowerConstants",
    Arg.Set Cil.lowerConstants,
    (" Lower constant expressions" ^ is_default !Cil.lowerConstants);

    "--noLowerConstants",
    Arg.Clear Cil.lowerConstants,
    (" Do not lower constant expressions" ^
       is_default (not !Cil.lowerConstants));

    "--insertImplicitCasts",
    Arg.Set Cil.insertImplicitCasts,
    (" Insert implicit casts" ^ is_default !Cil.insertImplicitCasts);

    "--noInsertImplicitCasts",
    Arg.Clear Cil.insertImplicitCasts,
    (" Do not insert implicit casts" ^
       is_default (not !Cil.insertImplicitCasts));

    "--forceRLArgEval",
    Arg.Set Cabs2cil.forceRLArgEval,
    (" Forces right to left evaluation of function arguments" ^
       is_default !Cabs2cil.forceRLArgEval);

    "--noForceRLArgEval",
    Arg.Clear Cabs2cil.forceRLArgEval,
    (" Evaluate function arguments in unspecified order" ^
       is_default (not !Cabs2cil.forceRLArgEval));

    "--nocil",
    Arg.Int (fun n -> Cabs2cil.nocil := n),
    "<index> Do not compile to CIL the global with the given index";

    "--noDisallowDuplication",
    Arg.Set Cabs2cil.allowDuplication,
    (" Duplicate small chunks of code if necessary" ^
       is_default !Cabs2cil.allowDuplication);

    "--disallowDuplication",
    Arg.Clear Cabs2cil.allowDuplication,
    (" Prevent small chunks of code from being duplicated" ^
       is_default (not !Cabs2cil.allowDuplication));

    "--keepunused",
    Arg.Set Rmtmps.keepUnused,
    (" Do not remove the unused variables and types" ^
       is_default !Rmtmps.keepUnused);

    "--nokeepunused",
    Arg.Clear Rmtmps.keepUnused,
    (" Remove unused variables and types" ^
       is_default (not !Rmtmps.keepUnused));

    "--rmUnusedInlines",
    Arg.Set Rmtmps.rmUnusedInlines,
    (" Delete any unused inline functions; this is the default in MSVC mode" ^
       is_default !Rmtmps.rmUnusedInlines);

    "--noRmUnusedInlines",
    Arg.Clear Rmtmps.rmUnusedInlines,
    (" Do not delete any unused inline functions" ^
       is_default (not !Rmtmps.rmUnusedInlines));

    (* Output Options *)
    "", Arg.Unit (fun () -> ()), " \n\t\tOutput Options\n";

    "--printCilAsIs",
    Arg.Set Cil.printCilAsIs,
    (" Do not try to simplify the CIL when printing." ^
       is_default !Cil.printCilAsIs);

    "--noPrintCilAsIs",
    Arg.Clear Cil.printCilAsIs,
    (" Simplify the CIL when printing.  This produces prettier output\n\t\t\t\tby e.g. changing while(1) into more meaningful loops  " ^ is_default (not !Cil.printCilAsIs));

    "--noWrap",
    Arg.Unit (fun _ -> Cil.lineLength := 100_000),
    " Do not wrap long lines when printing";

    "--pdepth",
    Arg.Int (fun n -> Pretty.printDepth := n),
    ("<n> Set max print depth (default: " ^
       string_of_int !Pretty.printDepth ^ ")");

    (* Don't just add new flags at the end ... place options
       in the correct category *)
  ]
