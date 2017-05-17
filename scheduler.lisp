;; =========================================================
;; =========================================================
;; Scheduler 
;; Amanda Ma
;; =========================================================
;; =========================================================
;; 
;; Holds all the functions to create a schedule 
;; 
;; LIST-OF-SCHEDULES - returns a list of random schedules 
;; SINGLE-SCHEDULE - returns a single schedule
;; CHECK-300 - checks if a schedule has required numbers of 300s 
;; CHECK-CLUSTERS - checks if a schedule satifies cluster requirements 





;; ===========================================================
;; =========================================================
;; SCHEDULE GENERATING FUNCTIONS 
;; =========================================================   
;; ===========================================================

;; single-schedule
;; --------------------------------------------------------
;; Input: list-of-classes - a list of classes 
;;        list-of-indexes - a list of indexes 
;; Output: a list where each index i holds 
;;         list-of-classes[list-of-indexes[i]] 
(defun single-schedule (major list-of-indexes)
  (let* ((non-req-classes (major-non-req-courses major)) 
	 (req-classes (major-req-courses major))
	 (schedule-length (length list-of-indexes))
	 (schedule (make-array schedule-length)))
    
    ;; Getting the actual schedule based on a list of
    ;; indexes to non-required classes 
    (dotimes (i schedule-length)
      (setf (aref schedule i) 
	(aref non-req-classes (aref list-of-indexes i))))
    
    ;; Set the schedule to be the full schedule of 
    ;; required classes + the combination 
    (setf schedule (concatenate 'array req-classes schedule))
    
    ;; check the schedule to see if it satifies basic constraints
    ;; return nil if it does not 
    (if (and (check-300 major schedule)
	     (check-clusters major schedule))
	(return-from single-schedule schedule))
    
    nil))
    

;; list-of-schedulues
;; --------------------------------------------------------
;; Input: A major 
;; Output: All the lists of possible schedules that will
;;         satisfy the major requirements withouth regards 
;;         to scheduling or preference 

(defun list-of-schedules (major)
  (let*
       ;; Number of classes in the major  
      ((num-total-classes (major-num-req-course major))
       ;; Number of classes required to take outside of required courses 
       (numclasses (- num-total-classes (length (major-req-courses major))))
       ;; Holds all the possible combinations of classes 
       (list-schedules nil)
       ;; Holds an array of indices to classes-to-choose. Will be used to
       ;; create a singular schedule 
       (schedule-index (make-array numclasses))
       (single-sched nil))
    
    ;; Get the initial schedule indices - will be (0, 1, 2) 
    (dotimes (i numclasses)
      (setf (aref schedule-index i) i))
    ;; Get the initial schedule 
    (setf single-sched (single-schedule major schedule-index)) 
    
    (while T
      ;; Will always have a schedule if reaching here, so
      ;; checks the schedule every time to see if it is valid,
      ;; if it is, push
      (if (not (null single-sched))
	  (push single-sched list-schedules))
      
      ;; get the rightmost index 
      (let ((j (- numclasses 1)))
	
	;; get the index that we'll be changing (will go from right to left)  
	(while (and (>= j 0) 
		    (= (aref schedule-index j) 
		       (+ (- num-total-classes numclasses) j)))
	  (decf j))
	
	;; If we get the the point where the indices are going negative,
	;; there are no more combinations, so return the list of schedules 
	(if (< j 0) 
	    (return-from list-of-schedules list-schedules))
	
	;; But if not, we increase the index of j (so for example if 
	;; j is the right-most positions, the index array will change
	;; from (1, 2, 3) to (1, 2, 4)
	(incf (aref schedule-index j))
	
	
	;; Then we will go rightwards if we need to change subsequent numbers 
	(incf j)
	(while (< j numclasses) 
	  (setf (aref schedule-index j)
	    (+ (aref schedule-index (- j 1)) 1))
	  (incf j))
	
	;; then set the schedule to be the new schedule  
	(setf single-sched (single-schedule major schedule-index)))))) 
    
    
;; check-300 
;; --------------------------------------------------------
;; Input: major - a major 
;;        schedule - a possible schedule 
;; Output: T if the schedule satifies the number of 300's required
;;         NIL if the schedule does not satisfy the number of 300's offered 
(defun check-300 (major schedule)
  (let ((num-300 (major-num-req-300 major))
	(count 0))
    ;; For everything in the schedule
    (dotimes (i (length schedule))
      ;; if the difficulty is 300, increase the count of 300-courses
      (if (equal (course-difficulty (aref schedule i)) 300)
	(incf count)))
      
    ;; Return if the number of 300 classes in the schedules satisfies
    ;; the minimun required for the major 
    (>= count num-300)))


;; check-clusters 
;; --------------------------------------------------------
;; Input: major - a major
;;        schedule -a possible schedule
;; Output: T if the schedule satifies the cluster requirement
;;         NIL if the schedule does not satisfy the cluster requirement 
(defun check-clusters (major schedule) 
  (let ((clusters (major-req-clusters major)))
    ;; for each list in the cluster
    (dolist (i clusters)
      (let ((in-cluster nil))
	;; get each element in that list of the cluster 
	(dolist (j i)
	  ;; get each element in the schedule
	  (dotimes(k (length schedule))
	    ;; check if something in the cluster is in the scedules
	    (if (equal j (aref schedule k))
		;; if so, the cluster is satisfied
		(setf in-cluster T))))
	;; If for a cluster, it hasn't been satisfied, then the schedule
	;; is invalid, return nil regardless of which cluster
	(if (not in-cluster)
	    (return-from check-clusters nil))))
    ;; If all the clusters are satisfied, return true 
    T))







;; ===========================================================
;; ===========================================================
;; SCHEDULE STRUCTURES AND FUNCTIONS 
;; ===========================================================
;; ===========================================================


;; Creating a structure for the schedule 
(defstruct (sem-schedule (:print-function show-schedule))
  ;; the array of courses 
  (schedule (make-array '(8 5) :initial-element nil)))

;; Printing the schedule out 
(defun show-schedule (state str depth)
  (declare (ignore depth))
  (let ((sched (sem-schedule-schedule state)))
    ;; For each semester 
    (dotimes (i 8)
      ;; Print out each semester 
      (format str "Semester ~A:~%" (+ i 1))
      ;; For each class in the semester 
      (dotimes (j 5)
	;; Print out the class. If it's occupied, put the class
	;; if not, it is a "free" class 
	(if (null (aref sched i j))
	    (format str "free~%")
	  (format str "~A" (aref sched i j)))))))



;; ===========================================================
;; ===========================================================
;;  BACKPATCHING FUNCTIONS - checking optimality of schedules 
;; =========================================================== 
;; ===========================================================


;; places-left-helper 
;; -------------------------------------------------------- 
;; Input: sem-schedule - a 2D array
;;        index - an integer detailing which semester 
;; Output: a boolean nil if it can't be placed in the schedule
;;         else, the position that it will be placed into the schedule 
(defun places-left-helper (sem-schedule index)
  (dotimes (i 5)
    (if (null (aref sem-schedule index i)) 
      (return-from places-left-helper i)))
  nil)


;; schedule-safe 
;; -------------------------------------------------------- 
;; Input: course - the course that one wants to put into the schedules 
;;        semester-schedulre - a schedule 
;;        index - the semester that one wants to put the course in 
;; Output: boolean if the course can be put into the schedule 
;;         based on pre-requisites and number of courses scheduled 
;;         in the semester 
(defun schedule-safe  (course semester-schedule index)
  (let ((pre-reqs (course-pre-reqs course))
	(sem-schedule (sem-schedule-schedule semester-schedule)))
    
    ;; for all required pre-reqs 
    (dotimes (i (length pre-reqs))
      (let ((sat-pre-req nil))
	;; check in each semester before where you want to put the class 
	(dotimes (j index)
	  ;; each class 
	  (dotimes (k 5)
	    ;; and see if it has been placed
	    (if (equal (svref pre-reqs i)
		       (aref sem-schedule j k))
		(setf sat-pre-req T))))
	(if (null sat-pre-req)
	    (return-from schedule-safe nil))))
    ;; if all pre-reqs are satisfied, and there is room in the semester,
    ;; return true
    (places-left-helper sem-schedule index)))



(defstruct sched-struct
  ;; a schedule representation for 4 years  
  schedule
  ;; the variables: the classes 
  class-var 
  ;; the values: times the class is offered 
  sem-times-val
  ;; the constraint for number of classes
  num-classes-const
  ;; the depth of the schedule so far
  depth 
  ;; the major the schedule is trying to satisfy  
  major)


(defun init-sched-struct (list-of-classes num-classes major)
  (let ((initial-struct (make-sched-struct))
	(sem-times (make-array (length list-of-classes) :initial-element nil)))
    (dotimes (i (length list-of-classes))
      (setf (aref sem-times i) (course-time (aref list-of-classes i))))
    
    (setf (sched-struct-schedule initial-struct) (make-sem-schedule))
    (setf (sched-struct-class-var initial-struct) list-of-classes)
    (setf (sched-struct-sem-times-val initial-struct) sem-times)
    (setf (sched-struct-num-classes-const initial-struct) num-classes)
    (setf (sched-struct-depth initial-struct) 0)
    (setf (sched-struct-major initial-struct) major)
    initial-struct))

(defun copy-sched-struct (s)
  (let ((copy-struct (make-sched-struct))
	(schedule (sched-struct-schedule s))
	(class-var (sched-struct-class-var s)) 
	(sem-times-val (sched-struct-sem-times-val s))
	(num-classes-const (sched-struct-num-classes-const s))
	(depth (sched-struct-depth s))
	(major (sched-struct-major s)))
    (setf (sched-struct-schedule copy-struct) schedule)
    (setf (sched-struct-class-var copy-struct) class-var)
    (setf (sched-struct-sem-times-val copy-struct) sem-times-val)
    (setf (sched-struct-num-classes-const copy-struct) num-classes-const)
    (setf (sched-struct-depth copy-struct) depth)
    (setf (sched-struct-major copy-struct) major)
    copy-struct))
	 
			

;; solve-schedule
;; -------------------------------------------------------- 
;; Input: list-of-classes - an array of courses 
;;        s - a semester struct 
;; Output: a schedule that is satisfactory for the course 
(defun solve-schedule (s)
  (cond 
   ;; Case 1: We have a full schedule 
   ((complete-sched? s)
    ;; return the schedule 
    s)
   
   ;; Case 2: Still trying to get a schedule 
   (t
    ;; Get the class that we change, this is inherently in order 
    ;; of required pre-requisites under the assumption that no
    ;; same-level courses are pre-requisites of one another
    (let* ((index (sched-struct-depth s))
	   ;; Gets the class
	   (class (aref (sched-struct-class-var s) index))
	   ;; Gets the times offered associated with that class 
	   (times (aref (sched-struct-sem-times-val s) index)))
      
      ;; if there are no values left in this domain, then it's not 
      ;; a valid solution
      (if (null times)
	  (return-from solve-schedule nil))
      
      
      ;; If there are values left in the domain, go through all of them
      ;; recursively 
      (dotimes (i (length times))
	;; create a child schedule with the change 
	(let ((child-s (do-move s class (svref times i))))
	  
	  ;; reduce the domains 
	  ;; if the child-s is not valid 
	  (if (null child-s)
	      ;; return nil
	      (return-from solve-schedule nil)
	    ;; if it is, reduce the domains 
	    (reduce-domains! child-s))
	  
	  ;; Recursive call to fill in the rest of the variables 
	  (let ((child-results (solve-schedule child-s)))
	    
	    ;; If we get a solution
	    (if T
		(return-from solve-schedule child-results)))))
      
      (return-from solve-schedule nil)))))
		
      

;; complete-sched?
;; -------------------------------------------------------- 
;; Input: s, a sched-struct
;; Output: a boolean seeing if the schedule is complete based 
;;         on the major requirements for number of courses 
(defun complete-sched? (s)
  (let ((num-major-courses (major-num-req-course (sched-struct-depth s))))
    ;; If the number of courses assigned to a schedule is as many
    ;; as what it required to complete the major, then the schedules is complete 
    (= (sched-struct-depth s) num-major-courses)))


;; do-move 
;; -------------------------------------------------------- 
;; Input: s, a sched-struct 
;;        class, a course
;;        sem-index, the semester in which you want to insert class in 
;; Output: a new sched-struct with the class inserted into the semester 
(defun do-move (s class sem-index)
  (let* (;; create a copy of the schedule struct 
	 (child-s (copy-sched-struct s))
	 ;; gets the index of where to put the class within the semester
	 (in-sem-index (schedule-safe class
				      (sched-struct-schedule s)
				      sem-index)))
    
    (cond
     ;; If the move is not possible, return nil 
     ((null in-sem-index)
      (format t "Move not possible - shouldn't get here!"))
     ;; Else 
     (t 
      ;; puts the class correctly in the right semester 
      (setf (aref (sched-struct-schedule child-s) sem-index in-sem-index) class)
      ;; increase the depth because we've added a class 
      (incf (sched-struct-depth child-s))
      ;; return the child node 
      child-s))))



;; reduce-domains!
;; --------------------------------------------------------   
;; Input: s, a sched-struct 
;; Output: s, destuctively modified to have consistent domains 
(defun reduce-domains! (s))
  
  
  