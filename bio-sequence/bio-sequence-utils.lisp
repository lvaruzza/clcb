(defgeneric shuffle-sequence (seq)
  (:documentation "Return a randomly shuffled copy of the sequence." )
  (:method ((seq trivial-bio-sequence)) 
    (let ((shuffled (copy-bio-sequence seq)))
      (setf (bio-sequence-seq shuffled)
            (nshuffle-vector (bio-sequence-seq shuffled)))
      shuffled)))


(defun orthogonal-coded (char)
  "Return the character as an orthogonal coded vector.  Vectors are
   orthogonal iff their scalar product equals zero. The scalar product of
   concatenations of othogonal coded bases gives the number of
   nucleotides that the sequences have in
   common.
   http://en.wikipedia.org/wiki/Orthogonal_array"
  (declare (type character char))
  (flet ((char-eq (c)
           (char-equal char c)))
    (the (simple-array single-float (*))
      (make-array 4 :initial-contents
                  (cond ((char-eq #\a) #(1f0 0f0 0f0 0f0))
                        ((char-eq #\c) #(0f0 1f0 0f0 0f0))
                        ((char-eq #\g) #(0f0 0f0 1f0 0f0))
                        ((char-eq #\t) #(0f0 0f0 0f0 1f0))
                        (t (error "Unknown nucleotide
character. Please extend `orthogonal-coded' to support this character.
")))
                  :element-type 'single-float))))

(defgeneric orthogonal-coded-seq (seq)
  ;;FIXME: Missing documentation
  (:documentation "Take an nucleotide sequence and generate an
   orthogonal coded copy of the sequence.  Albert, please describe what
   orthogonal coded means and when it is used."))

(defmethod orthogonal-coded-seq ((seq string))
  ;;FIXME: Missing documentation
  "Albert"
  (loop
     with orth-seq = (make-array (* (length seq) 4)
                                 :element-type 'single-float)
     for cur-pos fixnum from 0 below (length orth-seq) by 4
     for char character across seq do
     (setf (subseq orth-seq cur-pos (+ cur-pos 4))
           (orthogonal-coded char))
     finally (return orth-seq)))

(defun score-word (word scoring-word)
  "Albert"
  (reduce #'+ (map 'vector #'* word scoring-word)))
