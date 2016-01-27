#! /usr/bin/hy

; code-trace - Automatic multi-language code tracing
; created 16-01-24


(import argparse re)

(setv version "1.0")
(setv desc (+ "Dry run, simulate input, and log output and run state for source code (version " version ")"))

(defn parse-args []
      "Process file or directory name from command line
      16-01-24"
      (setv parser (.ArgumentParser argparse :description desc))
      (.add-argument parser :dest 'source :action 'store :default None :help "Source file or directory path")
      ;(.add-argument parser 'password :action 'store :default None :help "Populi password")
      (.parse-args parser)
      )

(defn entry-point [file]
      "Find execution start point to code.
      16-01-27"
      ; <process file or dir>
      (print "Entry point is:" file)
      file)

(defn file-type [file]
      ;(print "Assessing file type...")
      (cond [(.search re ".+[.]hy$" file) "Hy"]
            [(.search re ".+[.]py$" file) "Python"]
            [True "unknown"])
      )

(defn process-code [file]
      (setv type (file-type file))
      (for [line (open file)] (print (.rstrip line)))
      )

(defn main []
      "Main routine
      16-01-24"
      (setv args (parse-args))
      (setv init-file (entry-point (. args source)))
      (process-code init-file)
      )

(main)
(print "\n\nDONE!!!")