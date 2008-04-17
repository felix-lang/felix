(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
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

(* maincil *)
(* this module is the program entry point for the 'cilly' program, *)
(* which reads a C program file, parses it, translates it to the CIL *)
(* intermediate language, and then renders that back into C *)


module F = Frontc
module C = Cil
module CK = Check
module E = Errormsg
open Pretty
open Trace


let outChannel : out_channel option ref = ref None
let mergedChannel : out_channel option ref = ref None
let dumpFCG = ref false
let testcil = ref ""

exception Done_Processing


let parseOneFile (fname: string) : C.file =
  (* PARSE and convert to CIL *)
  if !Cilutil.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  
  if (not !Epicenter.doEpicenter) then (
    (* sm: remove unused temps to cut down on gcc warnings  *)
    (* (Stats.time "usedVar" Rmtmps.removeUnusedTemps cil);  *)
    (trace "sm" (dprintf "removing unused temporaries\n"));
    (Rmtmps.removeUnusedTemps cil)
  );
  cil

(** These are the statically-configured features. To these we append the 
  * features defined in Feature_config.ml (from Makefile) *)
  
let makeCFGFeature : C.featureDescr = 
  { C.fd_name = "makeCFG";
    C.fd_enabled = Cilutil.makeCFG;
    C.fd_description = "make the program look more like a CFG" ;
    C.fd_extraopt = [];
    C.fd_doit = (fun f -> 
      ignore (Partial.calls_end_basic_blocks f) ; 
      ignore (Partial.globally_unique_vids f) ; 
      Cil.iterGlobals f (fun glob -> match glob with
        Cil.GFun(fd,_) -> Cil.prepareCFG fd ;
                      (* jc: blockinggraph depends on this "true" arg *)
                      ignore (Cil.computeCFGInfo fd true)
      | _ -> ()) 
    );
    C.fd_post_check = true;
  } 

let features : C.featureDescr list = 
  [ Epicenter.feature;
    Ptranal.feature;
    Canonicalize.feature;
    Callgraph.feature;
    Logcalls.feature;
    Logwrites.feature;
    Heapify.feature1;
    Heapify.feature2;
    Oneret.feature;
    makeCFGFeature; (* ww: make CFG *must* come before Partial *) 
    Partial.feature;
    Simplemem.feature;
    Simplify.feature;
  ] 
  @ Feature_config.features 

let rec processOneFile (cil: C.file) =
  try begin

    if !Cilutil.doCheck then begin
      ignore (E.log "First CIL check\n");
      ignore (CK.checkFile [] cil);
    end;

    (* Scan all the features configured from the Makefile and, if they are 
     * enabled then run them on the current file *)
    List.iter 
      (fun fdesc -> 
        if ! (fdesc.C.fd_enabled) then begin
          if !E.verboseFlag then 
            ignore (E.log "Running CIL feature %s (%s)\n" 
                      fdesc.C.fd_name fdesc.C.fd_description);
          fdesc.C.fd_doit cil;
          (* See if we need to do some checking *)
          if !Cilutil.doCheck && fdesc.C.fd_post_check then begin
            ignore (E.log "CIL check after %s\n" fdesc.C.fd_name);
            ignore (CK.checkFile [] cil);
          end
        end)
      features;


    (* sm: enabling this by default, since I think usually we
     * want 'cilly' transformations to preserve annotations; I
     * can easily add a command-line flag if someone sometimes
     * wants these suppressed *)
    C.print_CIL_Input := true;

    (match !outChannel with
      None -> ()
    | Some c -> Stats.time "printCIL" 
	(C.dumpFile (C.defaultCilPrinter) c) cil);

    if !E.hadErrors then
      E.s (E.error "Cabs2cil has some errors");

  end with Done_Processing -> ()
        
(***** MAIN *****)  
let rec theMain () =
  let doCombine = ref "" in 
  let usageMsg = "Usage: cilly [options] source-files" in
  let fileNames : string list ref = ref [] in
  let recordFile fname = 
    fileNames := fname :: (!fileNames) 
  in
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
  in
  (* Processign of output file arguments *)
  let openFile (what: string) (takeit: out_channel -> unit) (fl: string) = 
    if !E.verboseFlag then
      ignore (Printf.printf "Setting %s to %s\n" what fl);
    (try takeit (open_out fl)
    with _ ->
      raise (Arg.Bad ("Cannot open " ^ what ^ " file")))
  in
  let outName = ref "" in
  let setDebugFlag v name =
    E.debugFlag := v;
    if v then Pretty.flushOften := true
  in
  (* sm: for print depth *)
  let setTraceDepth n =
    Pretty.printDepth := n
  in
  (*********** COMMAND LINE ARGUMENTS *****************)
  (* Construct the arguments for the features configured from the Makefile *)
  let blankLine = ("", Arg.Unit (fun _ -> ()), "") in
  let featureArgs = 
    List.fold_right
      (fun fdesc acc ->
	if !(fdesc.C.fd_enabled) then
          (* The feature is enabled by default *)
          blankLine ::
          ("--dont" ^ fdesc.C.fd_name, Arg.Clear(fdesc.C.fd_enabled), 
           " Disable " ^ fdesc.C.fd_description) ::
          fdesc.C.fd_extraopt @ acc
	else
          (* Disabled by default *)
          blankLine ::
          ("--do" ^ fdesc.C.fd_name, Arg.Set(fdesc.C.fd_enabled), 
           " Enable " ^ fdesc.C.fd_description) ::
          fdesc.C.fd_extraopt @ acc
      )
      features
      [blankLine]
  in
  let featureArgs = 
    ("", Arg.Unit (fun () -> ()), "\n\t\tCIL Features") :: featureArgs 
  in
    
  let argDescr = [
    "--verbose", Arg.Unit (fun _ -> E.verboseFlag := true),
                "turn on verbose mode";
    "--debug", Arg.String (setDebugFlag true),
                     "<xxx> turns on debugging flag xxx";
    "--flush", Arg.Unit (fun _ -> Pretty.flushOften := true),
                     "Flush the output streams often (aids debugging)" ;
    "--check", Arg.Unit (fun _ -> Cilutil.doCheck := true),
                     "turns on consistency checking of CIL";
    "--nocheck", Arg.Unit (fun _ -> Cilutil.doCheck := false),
                     "turns off consistency checking of CIL";
    "--nodebug", Arg.String (setDebugFlag false),
                      "<xxx> turns off debugging flag xxx";
    "--stats", Arg.Unit (fun _ -> Cilutil.printStats := true),
               "print some statistics";
    "--testcil", Arg.String (fun s -> testcil := s),
          "test CIL using the give compiler";
    "--nocil", Arg.Int (fun n -> Cabs2cil.nocil := n),
                      "Do not compile to CIL the global with the given index";
    "--disallowDuplication", Arg.Unit (fun n -> Cabs2cil.allowDuplication := false),
                      "Prevent small chunks of code from being duplicated";
    "--log", Arg.String (openFile "log" (fun oc -> E.logChannel := oc)),
             "the name of the log file";
    "--out", Arg.String (openFile "output" (fun oc -> outChannel := Some oc)),
             "the name of the output CIL file";
    "--warnall", Arg.Unit (fun _ -> E.warnFlag := true),
                 "Show all warnings";
    "--MSVC", Arg.Unit (fun _ ->   C.msvcMode := true;
                                   F.setMSVCMode ();
                                   if not Machdep.hasMSVC then
                                     ignore (E.warn "Will work in MSVC mode but will be using machine-dependent parameters for GCC since you do not have the MSVC compiler installed\n")
                       ), "Produce MSVC output. Default is GNU";
    "--stages", Arg.Unit (fun _ -> Cilutil.printStages := true),
               "print the stages of the algorithm as they happen";
    "--keepunused", Arg.Unit (fun _ -> Rmtmps.keepUnused := true),
                "do not remove the unused variables and types";

    "--mergedout", Arg.String (openFile "merged output"
                                   (fun oc -> mergedChannel := Some oc)),
                "specify the name of the merged file";
    "--ignore-merge-conflicts", 
                 Arg.Unit (fun _ -> Mergecil.ignore_merge_conflicts := true),
                  "ignore merging conflicts";
    "--noPrintLn", Arg.Unit (fun _ -> Cil.lineDirectiveStyle := None;
                                     Cprint.printLn := false),
               "don't output #line directives";
    "--commPrintLn", Arg.Unit (fun _ -> Cil.lineDirectiveStyle := Some Cil.LineComment;
                                       Cprint.printLnComment := true),
               "output #line directives as comments";
    "--printCilAsIs", Arg.Unit (fun _ -> Cil.printCilAsIs := true),
               "do not try to simplify the CIL when printing";
    "--sliceGlobal", Arg.Unit (fun _ -> Cilutil.sliceGlobal := true),
               "output is the slice of #pragma cilnoremove(sym) symbols";
    (* sm: some more debugging options *)
    "--tr",         Arg.String Trace.traceAddMulti,
                     "<sys>: subsystem to show debug printfs for";
    "--pdepth",     Arg.Int setTraceDepth,
                      "<n>: set max print depth (default: 5)";

    "--extrafiles", Arg.String parseExtraFile,
    "<filename>: the name of a file that contains a list of additional files to process, separated by whitespace of newlines";
  ] @ F.args @ featureArgs in
  begin
    (* this point in the code is the program entry point *)

    Stats.reset false; (* no performance counters *)

    (* parse the command-line arguments *)
    Arg.parse argDescr recordFile usageMsg;
    Cil.initCIL ();

    fileNames := List.rev !fileNames;

    if !testcil <> "" then begin
      Testcil.doit !testcil
    end else
      (* parse each of the files named on the command line, to CIL *)
      let files = List.map parseOneFile !fileNames in

      (* if there's more than one source file, merge them together; *)
      (* now we have just one CIL "file" to deal with *)
      let one =
        match files with
          [one] -> one
        | [] -> E.s (E.error "No arguments\n")
        | _ ->
            let merged =
              Stats.time "merge" (Mergecil.merge files)
                (if !outName = "" then "stdout" else !outName) in
            if !E.hadErrors then
              E.s (E.error "There were errors during merging\n");
            (* See if we must save the merged file *)
            (match !mergedChannel with
              None -> ()
            | Some mc -> begin
                let oldpci = !C.print_CIL_Input in
                C.print_CIL_Input := true;
                Stats.time "printMerged"
                  (C.dumpFile C.defaultCilPrinter mc) merged;
                C.print_CIL_Input := oldpci
            end);
            merged
      in

      (* process the CIL file (merged if necessary) *)
      processOneFile one
  end
;;
                                        (* Define a wrapper for main to 
                                         * intercept the exit *)
let failed = ref false 

let cleanup () = 
  if !E.verboseFlag || !Cilutil.printStats then
    Stats.print stderr "Timings:\n";
  if !E.logChannel != stderr then 
    close_out (! E.logChannel);  
  (match ! outChannel with Some c -> close_out c | _ -> ())

;;

try 
  theMain (); 
  cleanup ();
  exit (if !failed then 1 else 0)
with F.CabsOnly -> (* This is Ok *) fun () -> exit 0

cleanup ();

;;

