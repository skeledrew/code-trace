#! /usr/bin/hy

; code-trace - Automatic multi-language code tracing
; created 16-01-24


(import argparse re keyword)
(import ct-addon.hy)

(defclass CodeObject []
          "A queriable object of a source code file.
          16-01-27"
          
          [[name None]
           [contents None]
           [get-re (fn [self line] (if (.search re (setv regex (raw-input "Enter regex for: " line)) line) regex (.get-re self line)))]  ; only continue with a valid regex
           ; [get-mngr <code to manage newly found line>]
           ])

(defclass JavaObject [CodeObject]
          "Provides Java-specific methods for code.
          16-01-27"
          
          [[type "Java"]
           [keywords "abstract   continue   for          new         switch assert     default    if           package     synchronized boolean    do         goto         private     this break      double     implements   protected   throw byte       else       import       public      throws case       enum       instanceof   return      transient catch      extends    int          short       try char       final      interface    static      void class      finally    long         strictfp    volatile const      float      native       super       while"]
           [set-name (fn [self name] (setv (. self name) name))]
           [set-contents (fn [self contents]
                             (do (setv (. self contents) contents)
                                 (make-dom contents)))]
           ])

(defclass PythonObject [CodeObject]
          "Provides Python-specific methods for code.
          16-01-27"
          
          [[type "Python"]
           [--init-- (fn [self] (setv (. self keywords) (get-keywords self)) None)]  ; watch for error
           [get-keywords (fn [self] (for [word (. keyword kwlst)] (setv keywords (+ keywords word " "))))]
           ])

(setv code None)  ; dict of CodeObjects (name : obj)
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
      "Find execution start point to code. TODO
      16-01-27"
      ; <process file or dir>
      (print "Entry point is:" file)
      file)

(defn file-type [file]
      "Return the file type
      16-01-27"
      ;(print "Assessing file type...")
      (cond [(.search re ".+[.]hy$" file) "Hy"]
            [(.search re ".+[.]py$" file) "Python"]
            [True "unknown"])
      )

(defn code-tidy [lines type]
      "Ensure code is formatted to standards for easy parsing. TODO
      16-01-27"
      lines)

(defn process-code [file]
      "Create a code object from the given file and add it to the dictionary
      16-01-27"
      (print "Processing " file "...")
      (setv co (CodeObject))
      (setv (. co name) file)
      (setv (. co type ) (file-type file))
      (setv lines (open file))
      (setv (. co original) lines)  ; original file content
      (setv lines (code-tidy lines (. co type)))
      (setv (. co contents) lines)  ; tidied version
      (for [line lines] 
           (print (.rstrip line)))
      )

(defn main []
      "Main routine
      16-01-24, -27"
      (setv args (parse-args))
      (setv init-file (entry-point (. args source)))
      (process-code init-file)
      )

(main)
(print "\n\nDONE!!!")