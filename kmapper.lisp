;;;; kmapper.lisp
;;;; author: Currell Berry
;;;; This program creates karnaugh maps from truth table inputs.
;;;;
;;;; the truth table must be entered in whitespace separated form -- this is 
;;;; automatically done if you paste from a spreadsheet for example.
;;;;
;;;; note, "tlist" as referred to within the code below means a key value lisp p-list with the
;;;; column header as key and a list of the binary values associated with that
;;;; column header as values.

;;;some example truth tables

(defvar *truth-table* "
G	A	B	C	S0	S1	S2	W	X	Y	Z
0	0	0	0	0	0	0	0	1	0	0
1	0	0	0	0	0	1	0	1	0	0
0	0	0	1	0	1	0	0	1	0	1
1	0	0	1	1	0	0	0	1	0	1
0	0	1	0	0	1	1	0	0	0	1
1	0	1	0	0	1	1	0	0	0	1
0	0	1	1	0	1	1	0	1	1	0
1	0	1	1	1	1	1	0	1	1	0
0	1	0	0	1	0	1	0	0	1	1
1	1	0	0	1	0	1	0	0	1	1
0	1	0	1	1	1	1	0	0	0	0
1	1	0	1	1	0	1	0	0	0	0
0	1	1	0	X	X	X	X	X	X	X
1	1	1	0	X	X	X	X	X	X	X
0	1	1	1	0	0	0	1	0	0	1
1	1	1	1	0	0	0	1	0	0	1
")
(defvar *truth-table2* "
        A  B  C  O
        0  0  0  0
        0  0  1  0
        0  1  0  0
        0  1  1  1
        1  0  0  1
        1  0  1  1
        1  1  0  0
        1  1  1  1
")
(defvar *truth-table3*
  "
        A  B  C  D  O
        0  0  0  0  1
        0  0  0  1  0
        0  0  1  0  1
        0  0  1  1  0
        0  1  0  0  0
        0  1  0  1  0
        0  1  1  0  1
        0  1  1  1  0
        1  0  0  0  1
        1  0  0  1  1
        1  0  1  0  1
        1  0  1  1  1
        1  1  0  0  1
        1  1  0  1  0
        1  1  1  0  0
        1  1  1  1  0
")


;;; run this function to run the program
(defun create-k-maps (truth-table)
  (prog-runner (split-list->tlist (raw-ttable->split-list truth-table))))

(defun html-create-k-maps (truth-table numinputs)
  (html-prog-runner (split-list->tlist (raw-ttable->split-list truth-table))
		    numinputs))

(defun raw-ttable->split-list (truth-table) 
  "this function takes in the raw string representation and splits it into a 
'split' plist with keys :COLHEADERS and :REST "
  (let* ((ilist (%create-io-list truth-table))
	 (split-list 
	  (do ((olist nil)
	       (cur 0 (1+ cur)))
	      ((not (typep (car ilist) 'keyword)) (list :colheaders (reverse olist) :rest ilist))
	    (push (pop ilist) olist))))
  split-list))

(defun split-list->tlist (split-list)
  "This function takes the already partway parsed split-list
and converts it into a 'tlist' (whose format is described in the program
header"
    (let* ((temp-t-list (mapcan #'(lambda (key) (list key nil)) (getf split-list :colheaders)))
	   (numtoset (/ (length temp-t-list) 2))
	   (rlist (getf split-list :rest)))
      (dotimes (i numtoset)
	(setf (nth (+ 1 (* 2 i)) temp-t-list) (take-by-index-w-mod rlist i numtoset) ))
      temp-t-list))

(defun take-by-index-w-mod (ilist remainder modulus)
  (do ((index remainder (+ index modulus)) (olist nil))
  ((>= index (length ilist)) (reverse olist))
    (push (nth index ilist) olist)))

(defun getkeys (tlist)
  "returns the keys of a given tlist"
  (loop for n from 0 below (length tlist) by 2
       collect (nth n tlist)))

(defun getvals (tlist)
  "returns the vals (lists) of a given tlist"
  (loop for n from 1 below (length tlist) by 2
       collect (nth n tlist)))

(defun index-vals-by-row (tlistvals row-num)
  "use getvals on the basic tlist to get this going..."
  (let ((vals tlistvals))
    (loop for n from 0 below (length vals) by 1
       collect (nth row-num (nth n vals)))))

(defun select-row-by-kv (tlist kvpairs)
  "lets you select all matching items within a tlist given a 
set of key-value pairs to matching"
  (let* ((tlistvals (getvals tlist))
	 (tlistkeys (getkeys tlist))
	 (rowvals (rowsview tlistvals))
	(filteredrows (range :min 0 :max (length rowvals)))
	(keys (getf (dissoc-plist kvpairs) :keys))
	(vals (getf (dissoc-plist kvpairs) :vals)))
    (mapcar #'(lambda (key val) 
		(let* (
		       (index (position key tlistkeys)))
		  (loop for n from 0 below (length rowvals) by 1
		       do
		       (if (not (eql (nth index (nth n rowvals)) val))
			   (progn 
			     (setf filteredrows (remove n filteredrows)))
			   nil) ))) 
	    keys
	    vals)
    filteredrows))

(defun print-t-list (tlist)
  (format t "~{~5a~}~%" (getkeys tlist))
 
  (format t "~{~{~5a~}~%~}"  (rowsview (getvals tlist))))

(defun rowsview (tlistvals) 
  (loop for n from 0 below (length (car tlistvals)) by 1
	     collect (index-vals-by-row tlistvals n)))

(defun dissoc-plist (ilist)
  "splits a plist into its keys and vals parts"
  (do ((keyslist nil) (valslist nil) (i 0 (1+ i)))
      ((not (car ilist)) (list :keys keyslist :vals valslist))
    (push (pop ilist) keyslist)
    (push (pop ilist) valslist)))

(ql:quickload "cl-who")

(defun tester() (with-html-output-to-string (*standard-output* nil)
  (:table :border 0 :cellpadding 4
   (loop for i below 25 by 5
         do (htm
             (:tr :align "right"
              (loop for j from i below (+ i 5)
                    do (htm
                        (:td :bgcolor (if (oddp j)
                                        "pink"
                                        "green")
                             (fmt "~@R" (1+ j)))))))))))

(defun html-prog-runner (tlist numinputs) 
  "brings together logic of program." 
  (with-html-output-to-string (*standard-output* nil)
    (let* ((keys (getkeys tlist))
	   (numoutputs (- (length keys) numinputs))
	   (inputkeys (filter-by-index keys (range :min 0 :max numinputs)))
	   (outputs (filter-by-index keys (range :min numinputs :max (length keys))))
	   (leftinputs (filter-by-index inputkeys (range :min 0 :max (floor (/ numinputs 2)))))
	   (topinputs (filter-by-index inputkeys (range :min (length leftinputs) :max numinputs)))
	   (lcodes (create-gray-code (length leftinputs)))
	   (tcodes (create-gray-code (length topinputs))))
      (loop for outputwanted from 0 below numoutputs do
	   (fmt "~%")
	   (htm (:table :border 0 :cellpadding 4 
			(htm (
			:caption (fmt "~A k-map" (nth outputwanted outputs))))
			(let* ((oiqlist (nth outputwanted (get-outputvals tlist numinputs))))
			  (fmt "~%")
			  (htm (:tr (htm (:td (format t "~a " inputkeys)))
				    (loop for tcode in tcodes do
					 (htm (:td (fmt "~a" tcode)))))) 
			  (loop for lcode in lcodes do
			       (fmt "~%")
			       (htm (:tr 
				     (htm (:td (fmt "~a" lcode))
					  (loop for tcode in tcodes do 
					       (htm (:td (fmt "~a" (nth 
							  (car 
							   (select-row-by-kv 
							    tlist 
							    (zip-plist inputkeys (append lcode tcode))))
							  oiqlist))))))))))))))))

(defun prog-runner (tlist) 
  "brings together logic of program." 
  (let* ((keys (getkeys tlist))
	 (numinputs (parse-integer (prompt-read "Number of Inputs?")))
	 (numoutputs (- (length keys) numinputs))
	 (inputkeys (filter-by-index keys (range :min 0 :max numinputs)))
	 (outputs (filter-by-index keys (range :min numinputs :max (length keys))))
	 (leftinputs (filter-by-index inputkeys (range :min 0 :max (floor (/ numinputs 2)))))
	 (topinputs (filter-by-index inputkeys (range :min (length leftinputs) :max numinputs)))
	 (lcodes (create-gray-code (length leftinputs)))
	 (tcodes (create-gray-code (length topinputs))))
    (loop for outputwanted from 0 below numoutputs do
	 (format t "~a k-map~%" (nth outputwanted outputs))
	 (let* ((oiqlist (nth outputwanted (get-outputvals tlist numinputs))))
	   (format t "~10a " inputkeys)
	   (loop for tcode in tcodes do
	      (format t "~10a" tcode))
	 (format t "~%")
	 (loop for lcode in lcodes do
	      (format t "~10a ~{~10a~}~%" 
		      lcode 
		      (loop for tcode in tcodes collecting
			    (nth 
			     (car 
			      (select-row-by-kv 
			       tlist 
			       (zip-plist inputkeys (append lcode tcode))))
			     oiqlist)))))

    (format t "~%"))))

(defun filter-by-index (list indexes)
    (loop for item in indexes collecting
	 (nth item list)))

(defun create-gray-code (numbits) 
  "wrapper for %create-gray-code"
  (%create-gray-code '((0) (1)) 1 numbits))

(defun %create-gray-code (prev-code currentlevel finallevel) 
  "given 1. a gray code as a list of lists, 2. the current number of bits in this gray code,
  and 3. the number of bits we want in our final gray code, this function recursively generates
  this 'finallevel' bit gray code.  uses logic found in in the wikipedia article on gray codes."
  (if (eql currentlevel finallevel)
      prev-code ;;base case!  we're done
      (let* ((from-old (mapcar #'(lambda (x) (cons 0 x)) prev-code)) ;;prefix old entries with 0
	     (from-new (mapcar #'(lambda (x) (cons 1 x)) (reverse prev-code))) ;;reflect old entries, prefix these new entries with 1
	     (new-code (nconc from-old from-new)))
	(%create-gray-code new-code (+ currentlevel 1) finallevel)))) ;recurse!

(defun get-outputvals (tlist numinputs)
  (let ((vals (getvals tlist)))
    (filter-by-index vals (range :min numinputs :max (length vals)))))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun %create-io-list (s-truth-table)
  (with-input-from-string 
      (s s-truth-table)
    (loop for x = (read s nil :end) until (eq x :end) 
       collect (if 
		(typep x 'symbol) 
		(make-keyword x)
		x)) ))

;;; utility code
(defmacro break-transparent (exp)
  `(let ((x ,exp)) (break "argument to break: ~:S" x) x))

(defun range (&key (min 0) (max 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (read-line *query-io*))

(defun zip-plist (keys values)
  "creates a plist from a list of keys and a list of values."
  (loop for k in keys
        for v in values nconc
       (list k v)))


;; (defun %tester1 () 
;;   (let* ((outputwanted 0)
;; 	(oiqlist (nth outputwanted (get-outputvals *itlist* 4)))
;; 	(lcodes (create-gray-code 2))
;; 	(tcodes (create-gray-code 2)))
;;     (loop for lcode in lcodes collecting
;; 	 (loop for tcode in tcodes collecting
;; 	      (cons 
;; 	       (append lcode tcode) 
;; 	       (nth 
;; 		(car 
;; 		 (select-row-by-kv 
;; 		  *itlist* 
;; 		  (zip-plist '(:G :A :B :C) (append lcode tcode))))
;; 		oiqlist))))))
