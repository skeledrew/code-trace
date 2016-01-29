#! /usr/bin/hy

; code-trace - Automatic multi-language code tracing
; created 16-01-24


(import argparse re keyword [bs4 [BeautifulSoup]])

(defclass CodeObject []
          "A queriable object of a source code file.
          16-01-27"
          
          [[name None]
           [type "unknown"]
           [keywords ""]
           [original []]  ; original code
           [contents ""]  ; tidied code
           [soup None]  ; code in soup object
           [get-re (fn [self line] (if (.search re (setv regex (raw-input "Enter regex for: " line)) line) regex (.get-re self line)))]  ; only continue with a valid regex
           ; [get-mngr <code to manage newly found line>]
           [set-name (fn [self name] (setv (. self name) name))]
           [add-code (fn [self original] (setv (. self original) original) (.set-contents self (.code-tidy self original)))]
           [code-tidy (fn [self code] code)]  ; tidy stub
           [set-contents (fn [self contents] (setv (. self contents) contents) (.make-soup self))]
           [make-soup (fn [self] (setv soup (BeautifulSoup "<CodeObject/>" "xml"))
                          ;(.append (. soup CodeObject) (.new-tag soup "Type"))
                          (assoc (. soup CodeObject) "name" (. self name) "type" "unknown")
                          (setv processors (load-processors processor-file-name))  ; load dynamic processing code
                          (for [line (. self contents)]
                               (print (.rstrip line)))
                          (print "\nSoup content: " (get (. soup contents) 0)))]
           ])

(defclass JavaObject [CodeObject]
          "Provides Java-specific methods for code.
          16-01-27"
          
          [[type "Java"]
           [keywords "abstract   continue   for          new         switch assert     default    if           package     synchronized boolean    do         goto         private     this break      double     implements   protected   throw byte       else       import       public      throws case       enum       instanceof   return      transient catch      extends    int          short       try char       final      interface    static      void class      finally    long         strictfp    volatile const      float      native       super       while"]
           ])

(defclass PythonObject [CodeObject]
          "Provides Python-specific methods for code.
          16-01-27"
          
          [[type "Python"]
           [--init-- (fn [self] (.get-keywords self) None)]
           [get-keywords (fn [self] (setv keywords "") (for [word (. keyword kwlist)] (setv keywords (+ keywords word " "))) (setv (. self keywords) keywords))]
           ])

(setv code None)  ; dict of CodeObjects (name : obj)
(setv processor-file-name "ct-addon.hy")
(setv ct-addon "")
(setv version "1.0")
(setv desc (+ "Dry run, simulate input, and log output and run state for source code (version " version ")"))

(defn load-processors [file]
      "Load expressions to get the regex, test and operation
      16-01-28"
      (setv lines (open file))
      (setv ct-addon lines)
      (setv code-lines [])
      (for [line lines]
           (if (.search re ".*---STOP---.*" line) (break))
           ;(if (.search re "\s*(import.*" line) (continue))  ; skip import statements
           (if (.search re "^[ ]*[(][^ ]+" line) (.append code-lines (.rstrip line))))
      code-lines)

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

(defn get-co [file]
      "Return the file type
      16-01-27"
      ;(print "Assessing file type...")
      (cond ;[(.search re ".+[.]hy$" file) (HyObject)]
            [(.search re ".+[.]py$" file) (PythonObject)]
            [True (CodeObject)])
      )

(defn code-tidy [lines type]
      "Ensure code is formatted to standards for easy parsing. TODO
      16-01-27"
      lines)

(defn process-code [file]
      "Create a code object from the given file and add it to the dictionary
      16-01-27"
      (print (+ "Processing " file "..."))
      (setv co (get-co file))  ; get a code object for the appropriate file type
      (print "Detected type is" (. co type))
      (.set-name co file)
      (setv lines (open file))
      (.add-code co lines)  ; original file content
      (setv lines (. co contents))  ; tidied version
      ;(for [line lines] 
      ;     (print (.rstrip line)))
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