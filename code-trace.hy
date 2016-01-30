#! /usr/bin/hy

; code-trace - Automatic multi-language code tracing
; created 16-01-24

" Code Trace Specifications
created 16-01-29

Requirements
- read source code content into an XML DOM
-- read file contents
-- store original version
-- tidy code and store
-- markup code with XML and store
"

(import argparse re keyword [bs4 [BeautifulSoup]] os sys io inspect)

(defclass CodeObject2 [BeautifulSoup]
          "A queriable object of a source code file.
          v2 - inherit from BeautifulSoup
          16-01-27"
          
          [[--init-- (fn [self &optional [code "<CodeObject type=\"unknown\"/>"]]
                         (print "Constructing CodeObject")
                         (setv processors (load-processors processor-file-name))  ; load dynamic processing code
                         (setv domize-code "")
                         ;(for [processor processors] (if (.search re ".*-domize- .*" processor) (do (setv domize-code (.rstrip processor)) (exec-code domize-code) (break))))
                         (for [line (. self contents)]
                               ;(.append (. self CodeObject) (.-domize- self line "" (. self type)))
                               (print (.rstrip line)))
                         ;(print "\nSoup content: " (get (. self contents) 0))
                         )]  ; initialize with code object dom
           [orig-code []]  ; original code
           [tidy-code []]  ; tidied code
           [contents ""]  ; DEBUG: was giving errors in objects without this
           [-domize- (fn [self 2 3 4] None)]  ; define name of dynamic function, replace with function loaded from string
           [get-re (fn [self line] (if (.search re (setv regex (raw-input "Enter regex for: " line)) line) regex (.get-re self line)))]  ; only continue with a valid regex
           ; [get-mngr <code to manage newly found line>]
           [add-code (fn [self original] (setv (. self original) original) (.set-contents self (.code-tidy self original)))]
           [code-tidy (fn [self code] code)]  ; tidy stub, override in descendants
           [set-contents (fn [self contents] (setv (. self contents) contents) (.make-soup self))]
           [set-fname (fn [self file-name] (assoc self "filename" file-name))]
           [get-fname (fn [self] (get (. self name) "filename"))]
           [get-name (fn [self] (. self name))]
           ])

(defclass CodeObject []
          "A queriable object of a source code file. Obselete, use version 2
          v1 - use BeautifulSoup internally
          16-01-27"
          
          [[name None]
           [type "unknown"]
           [keywords ""]
           [original []]  ; original code
           [contents ""]  ; tidied code
           [soup None]  ; code in soup object
           [-domize- (fn [self 2 3 4] None)]  ; define name of dynamic function
           [get-re (fn [self line] (if (.search re (setv regex (raw-input "Enter regex for: " line)) line) regex (.get-re self line)))]  ; only continue with a valid regex
           ; [get-mngr <code to manage newly found line>]
           [set-name (fn [self name] (setv (. self name) name))]
           [add-code (fn [self original] (setv (. self original) original) (.set-contents self (.code-tidy self original)))]
           [code-tidy (fn [self code] code)]  ; tidy stub
           [set-contents (fn [self contents] (setv (. self contents) contents) (.make-soup self))]
           [make-soup (fn [self] (setv soup (BeautifulSoup "<CodeObject/>" "xml"))
                          (assoc (. soup CodeObject) "name" (. self name) "type" "unknown")
                          (setv processors (load-processors processor-file-name))  ; load dynamic processing code
                          (setv domize-code "")
                          ;(for [processor processors] (if (.search re ".*-domize- .*" processor) (do (setv domize-code (.rstrip processor)) (exec-code domize-code) (break))))
                          (for [line (. self contents)]
                               ;(.append (. soup CodeObject) (.-domize- self line "" (. self type)))
                               (print (.rstrip line)))
                          (print "\nSoup content: " (get (. soup contents) 0)))]
           ])

(defclass JavaObject [CodeObject]
          "Provides Java-specific methods for code.
          16-01-27"
          
          [[type "Java"]
           [keywords "abstract   continue   for          new         switch assert     default    if           package     synchronized boolean    do         goto         private     this break      double     implements   protected   throw byte       else       import       public      throws case       enum       instanceof   return      transient catch      extends    int          short       try char       final      interface    static      void class      finally    long         strictfp    volatile const      float      native       super       while"]
           ])

(defclass PythonObject [CodeObject2]
          "Provides Python-specific methods for code.
          16-01-27"
          
          [[--init-- (fn [self] 
                         (print "Constructing PythonObject") 
                         (.--init-- CodeObject2 self) 
                         (.get-keywords self) 
                         ;(assoc (. self CodeObject) 'type 'Python) 
                         None)]
           [get-keywords (fn [self] (setv keywords "") (for [word (. keyword kwlist)] (setv keywords (+ keywords word " "))) (setv (. self keywords) keywords))]
           [get-type (fn [self] "Python")]
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
      ;(print "DEBUG:" file "\n" lines "\n")
      (for [line lines]
           (if (.search re ".*---STOP---.*" line) (break))
           ;(if (.search re "\s*(import.*" line) (continue))  ; skip import statements
           (if (.search re "^[ ]*[(][^ ]+" line) (.append code-lines (.rstrip line))))  ; BUG: Seems Hy doesn't like brackets & parens in strings...
      code-lines)

(defn parse-args []
      "Process file or directory name from command line
      16-01-24"
      (setv parser (.ArgumentParser argparse :description desc))
      (.add-argument parser :dest 'source :action 'store :default None :help "Source file or directory path")
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
      "Ensure code is formatted to standards for easy parsing. Obselete, use method version
      16-01-27"
      lines)

(defn process-code [file]
      "Create a code object from the given file and add it to the dictionary
      16-01-27"
      (print (+ "Processing " file "..."))
      (setv co (get-co file))  ; get a code object for the appropriate file type
      (print "Detected type is" (.get-type co))
      (setv (. co name) "CodeObject")
      ;(setv (. co contents) "<bleh num=\"hey!\"/>")
      ;(assoc (. co CodeObject) 'type 'Python)
      ;(for [item (.getmembers inspect co)] (print item))
      (print "\n" (.prettify co))
      ;(.set-fname co file)
      (setv lines (open file))
      ;(.add-code co lines)  ; original file content
      ;(setv lines (. co contents))  ; tidied version
      )

(defn mem-file [text]
      "A workaround to help get eval to process a string by returning the given text in a pipe. Obselete, use exec-code instead *shoot myself for not seeing that sooner...*
      https://docs.python.org/2/library/os.html#os.read
      http://www.tutorialspoint.com/python/os_pipe.htm
      https://docs.python.org/2/library/os.html#os.fork
      http://www.tutorialspoint.com/python/file_read.htm
      16-01-29"
      (setv my-pipe (.pipe os))
      (setv pid (.fork os))
      (if pid (do (.close os (get my-pipe 1)) (setv r (.fdopen os (get my-pipe 0))) (setv str (.read r)) (print "Found:" str) r)
          (do (.close os (get my-pipe 0)) (setv w (.fdopen os (get my-pipe 1) "w")) (.write w text) (.close w) (.exit sys 0)))
      )

(defn exec-code [str]
      "Evaluates the given string
      16-01-29"
      (eval (apply read [] {"from_file" (.StringIO io str)})))

(defn main []
      "Main routine
      16-01-24, -27"
      (setv args (parse-args))
      (setv init-file (entry-point (. args source)))
      (process-code init-file)
      )

(main)
(print "\nDONE!!!")


(print "\n\n\n===TEST SECTION===")
(defclass BSObject [BeautifulSoup]
          [])

;(setv tco (BeautifulSoup "<meek type=\"blank\"/>" 'xml))
(setv tco (BSObject))
;(setv (. tco meek name) "defl")
;(print (. tco meek name))
;(assoc (. tco meek) "type" "changed!")
;(.append (. tco meek) "Kool Stuff")
(print (.prettify tco))

(print "REALLY done now.")
