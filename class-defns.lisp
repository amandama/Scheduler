;; =========================================================  
;; =========================================================  
;; class-defns.lisp
;; Amanda Ma 
;; =========================================================
;; =========================================================  
;; Structures for classes and majors 
;; Also creates information on each class and major 
;; including the CS major 



(defstruct (course (:print-function show-classes)) 
  name        ;; name of the class 
  time        ;; array of each semester the class is offered 
  pre-reqs    ;; vector of classes that are prerequisites to this class 
  difficulty) ;; either 100, 200, or 300 

(defun show-classes (state str depth)
  (declare (ignore depth))
  (format str "~A~%" (course-name state)))


(defstruct major
  name            ;; the name of a major  
  num-req-course  ;; number of courses required for the major
  num-req-300     ;; the number of 300 levels one is required to take  
  req-courses     ;; an array of the classes one is required to take
  non-req-courses ;; an array of the classes on is not required to take,
                  ;; req-courses U non-req-course = all courses 
  req-clusters)   ;; a 2-d array, where each row is a cluster in the major 
                  ;; (students have to take at least one class in each cluster) 




;; Creating numerical values for each semester 
(defconstant f13 0) ;; fall 2013 
(defconstant s14 1) ;; spring 2014
(defconstant f14 2) ;; fall 2014 
(defconstant s15 3) ;; spring 2015 
(defconstant f15 4) ;; fall 2015 
(defconstant s16 5) ;; spring 2016
(defconstant f16 6) ;; fall 2016 
(defconstant s17 7) ;; spring 2017


(defconstant 100-level 100)
(defconstant 200-level 200)
(defconstant 300-level 300) 



;; ============================================================================ 
;;  COMPUTER SCIENCE MAJOR 
;; ============================================================================
;; The classes offered by the computer science department 

(defconstant CS101 
    (make-course :name "cs101 - Computer Science I: Problem-Solving and Abstraction"
		 :time (vector f13 s14 f14 s15 f15 s16 f16 s17)
		 :pre-reqs NIL
		 :difficulty 100-level))

(defconstant CS102 
    (make-course :name "cs102 - Computer Science II: Data Structures and Algorithms"
		 :time (vector f13 s14 f14 s15 f15 s16 f16 s17)
		 :pre-reqs (vector cs101)  
		 :difficulty 100-level)) 

(defconstant CS145
    (make-course :name "cs145 - Foundations of Computer Science"
		 :time (vector f13 s14 f14 s15 f15 s16 f16 s17)
		 :pre-reqs (vector cs101) 
		 :difficulty 100-level))

(defconstant CS203 
    (make-course :name "cs203 - Computer Science III: Software Design"
		 :time (vector f13 s14 f14 s15 f15 s16 f16 s17)
		 :pre-reqs (vector cs102)
		 :difficulty 200-level))

(defconstant CS224
    (make-course :name "cs224 - Computer Organization"
		 :time (vector s14 f14 s15 f15 s16 f16 s17)
		 :pre-reqs (vector cs102 cs145)
		 :difficulty 200-level))

(defconstant CS235
    (make-course :name "cs235 - Programming Languages"
		 :time (vector f14)
		 :pre-reqs (vector cs102 cs145)
		 :difficulty 200-level))

(defconstant CS240
    (make-course :name "cs240 - Language Theory and Computation"
		 :time (vector f13 f14 s15 f15 s16 f16 s17)
		 :pre-reqs (vector cs102 cs145)
		 :difficulty 200-level))

(defconstant CS241
    (make-course :name "cs241 - Analysis of Algorithms"
		 :time (vector f13 f14 s15 f15 s16 f16 s17)
		 :pre-reqs (vector cs102 cs145)
		 :difficulty 200-level))


(defconstant CS245
    (make-course :name "cs245 - Declarative Programming Models"
		 :time (vector s14 s16 f16)
		 :pre-reqs (vector cs102 cs145)
		 :difficulty 200-level))

(defconstant CS250
    (make-course :name "cs250 - Modeling, Simulation and Analysis"
		 :time (vector s15 f15 f16)
		 :pre-reqs (vector cs102)
		 :difficulty 200-level))


(defconstant CS324
    (make-course :name "cs324 - Computer Architecture"
		 :time (vector s15 s17)
		 :pre-reqs (vector cs224)
		 :difficulty 300-level))

(defconstant CS331
    (make-course :name "cs331 - Compilers"
		 :time (vector s14 s15 s16 s17)
		 :pre-reqs (vector cs224 cs240)
		 :difficulty 300-level))

(defconstant CS334
    (make-course :name "cs334 - Operating Systems"
		 :time (vector f13 f14 f15 s16 f16)
		 :pre-reqs (vector cs203 cs224)
		 :difficulty 300-level))

(defconstant CS353
    (make-course :name "cs353 - BioInformatics"
		 :time (vector s14 s16)
		 :pre-reqs (vector cs203)
		 :difficulty 300-level))

(defconstant CS365
    (make-course :name "cs365 - Artificial Intelligence"
		 :time (vector f14 s17)
		 :pre-reqs (vector cs145 cs203 cs245)
		 :difficulty 300-level))

(defconstant CS366
    (make-course :name "cs366 - Computational Linguistics"
		 :time (vector s14 s16)
		 :pre-reqs (vector cs240)
		 :difficulty 300-level))

(defconstant CS375
    (make-course :name "cs375 - Networks"
		 :time (vector f13 f15)
		 :pre-reqs (vector cs203)
		 :difficulty 300-level))

(defconstant CS376
    (make-course :name "cs376 - Computer Games: Design, Production and Critique"
		 :time (vector f13 f15)
		 :pre-reqs (vector cs203)
		 :difficulty 300-level))

(defconstant CS377
    (make-course :name "cs377- Parallel Programming"
		 :time (vector s15 f16)
		 :pre-reqs (vector cs203 cs224)
		 :difficulty 300-level))

(defconstant CS378
    (make-course :name "cs378 - Graphics"
		 :time (vector f13 f15)
		 :pre-reqs (vector cs203)
		 :difficulty 300-level))

(defconstant CS379
    (make-course :name "cs395- Computer Animation: Art, Science and Criticism"
		 :time (vector f14 f16)
		 :pre-reqs (vector cs203)
		 :difficulty 300-level))


;; The computer science major 
(defconstant cs-major 
    (make-major :name "Computer Science" 
		:num-req-course 12       ;; Ignoring math requirement 
		:num-req-300 4          
		:req-courses 
		(make-array 9 
			    :initial-contents
			    (list CS101 CS102 CS145 CS203 CS224 
				  CS240 CS241 CS331 CS334))
		:non-req-courses
		(make-array 12
			    :initial-contents 
			    (list CS235 CS245 CS250 CS324 CS353 CS365 
				  CS366 CS375 CS376 CS377 CS378 CS379))
		:req-clusters 
		(list (list CS235 CS245 CS250)
		      (list CS324 CS365 CS366 CS375 CS377 CS378))))
