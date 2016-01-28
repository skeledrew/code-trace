#! /usr/bin/hy

; Self-modifying extender file
; In order to prevent the main code-trace source file from becoming polluted, this will be called and used to develop handling routines on the fly.

(import re [bs4 [BeautifulSoup]])


; Get a valid regular expression. Create if main method fails.


; Get a test output for assertion, eg. resulting XML. Run through BS for validity.
;(defn -test- [-code- -expected-]

; Get code for handling lines matching given expression and type. Test code by calling with line and asserting.
(defn -domize- [-line- -regex- -type-] (cond [(and (.search re (+ ".*" -type- ".*") "Python Bash Hy Perl") (.search re "\s*[#][!].+" -line-)) (setv ret (+ "<hashbang>" (get (.findall re "\s*[#][!]\s*(.+)") 0) "</hashbang>"))]))
; comments regex (doesn't match hashbang): \s*[#][^!].+
; blank line (starts & ends with 0+ non-ws): ^\s*$



; ---STOP---

(defn code-lines [file]
      "Open file and find relevant code lines
      16-01-27"
      (setv lines (open file))
      ;(setv count 0)
      (setv code-lines [])
      (for [line lines]
           (if (.search re ".*---STOP---.*" line) (break))
           (if (.search re "^[ ]*[(][^ ]+" line) (.append code-lines (.rstrip line))))
      code-lines)

(defmain [&rest args]
         "Self-test call to print line #s with actual code
         16-01-27"
         (print "Lines with code:" (code-lines (get args 0)))
         )
