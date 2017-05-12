;; =========================================================
;; =========================================================
;; Scheduler 
;; Amanda Ma
;; =========================================================
;; =========================================================
;; Holds all the functions to create a schedule 




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
    ;; (if (not (or (check-300 major schedule)
    ;;              (check-clusters major schedule)))
    ;;     NIL)
   
    
    schedule))
    



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
       (schedule-index (make-array numclasses)))
    
    ;; Get the initial schedule indices - will be (0, 1, 2) 
    (dotimes (i numclasses)
      (setf (aref schedule-index i) i))
    ;; Push the initial schedule 
    (push (single-schedule major schedule-index) list-schedules)
    
    (while T
      
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
	
	;; then push the schedule onto the list of schedules if it it valid 
	(push (single-schedule major schedule-index) 
	      list-schedules)))))
       
   
    
    
    
;; check-300 
;; --------------------------------------------------------
;; Input: major - a major 
;;        schedule - a possible schedule 
;; Output: T if the schedule satifies the number of 300's required
;;         NIL if the schedule does not satisfy the number of 300's offered 
(defun check-300 (major schedule)
  (let ((num-300 (major-num-req-300 major))
	(count 0))
    (dotimes (i (length schedule))
      (if (= (course-difficulty (aref schedule i)) 300)
	  (incf count)))
    
    (>= count num-300)))


;; check-clusters 
;; --------------------------------------------------------
;; Input: major - a major
;;        schedule -a possible schedule
;; Output: T if the schedule satifies the cluster requirement
;;         NIL if the schedule does not satisfy the cluster requirement 


    