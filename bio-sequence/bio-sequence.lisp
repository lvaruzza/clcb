;;;; Copyright (c) 2007 Albert Krewinkel
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package #:clcb)

(defclass abstract-bio-sequence (molecule abstract-interval)
  ((id
    :accessor bio-sequence-id
    :initarg :id
    :documentation "Unique identifier of the sequence.  In cases where
    an sequence is read from a database, this will often be the
    primary key under which the sequence is filed.")
   (seq
    :accessor bio-sequence-seq
    :accessor seq
    :initarg :seq
    :initform nil
    :type sequence
    :documentation "The actual sequence information. This can be any
    object of type sequence, but will normally be of type
    `string'.")
   (direct-superseq
    :accessor direct-superseq
    :initarg direct-superseq
    :initform nil
    :type (or nil abstract-bio-sequence)
    :documentation "Bio-sequence to which this object is relative to.
    May be the object itself.")
   (direct-subseqs
    :accessor direct-subseqs
    :initarg :subseqs
    :type sequence
    :documentation ""))
  (:documentation "This is the base class for all types of biological
   polymeric macromolecules which are build by a sequence of
   monomeres.  This includes nucelic acids as well as proteins and
   peptides.

   In daily routine, one does not use the full sequence but some
   fraction of it. And in this fraction we are interested in smaller
   fractions that have (or are presumed to have) properties of our
   interest. These regions, consecutive stretches within something
   larger, may handily be understood as intervals and CLCB offers such
   an interface to them."))

(defclass trivial-bio-sequence (abstract-bio-sequence integer-interval)
  ((lower
    :accessor seq-start
    :initarg :seq-start
    :initform 1
    :documentation "Start position of the sequence.  Particularly
    those sequences that are part of another sequence may be
    preferably start their numbering with the parental position.
    Defaults to 1."  )
   (upper
    :accessor seq-end
    :initarg :seq-end
    :documentation "End position of the sequence.  If this slot isn't
    set explicetly, it will be set to the length of the `seq' slot.")))


(defclass fragmented-bio-sequence (abstract-bio-sequence)
  ((seq-fragments
    :accessor seq-fragments
    :accessor intervals     ; For compliance with the interval protokoll
    :initarg :seq-fragments
    :type sequence
    :initform nil
    :documentation "The fragments of this sequence, each one being a
    bio-sequence itself."))
  (:documentation "A bio sequence which is constructed from two or
  more elementary sequences.  A well known examples for this would be
  a spliced mRNA, which is constituted by a number of exons."))


(defmethod initialize-instance :after ((seq trivial-bio-sequence) &rest args)
  (declare (ignore args))
  (unless (slot-boundp seq 'upper)
    (setf (seq-end seq) (length (bio-sequence-seq seq))))
  ;; If no superseq was specified, set it to be the sequence itself.
  (unless (slot-boundp seq 'direct-superseq)
    (setf (direct-superseq seq) seq)))

;;; Printing
(defmethod print-object ((object trivial-bio-sequence) (stream stream))
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "\"~A\" [~A,~A]"
            (bio-sequence-id object)
            (seq-start object)
            (seq-end object))))


(defun super-sequences (bio-sequence)
  "Return a list of super-sequences, where each list element is the
  superseq of is predecessor (if any)."
  (declare (type abstract-bio-sequence bio-sequence))
  (let ((direct-superseq (direct-superseq bio-sequence)))
    (if (or (eq (direct-superseq direct-superseq) direct-superseq)
            (null direct-superseq))
        (list direct-superseq)
        (cons direct-superseq (super-sequences direct-superseq)))))


(defclass bio-sequence-record (trivial-bio-sequence)
  ((name :accessor bio-sequence-name
         :initarg :name
         :initform "Unknown"
         :documentation "The sequence's name.")
   (description :accessor bio-sequence-description
                :initarg :description
                :documentation "The descripitions of the object."))
   (:documentation "A biological sequence is a polymeric macromolecule
    of nucelic acids (with a phorsphor-sugar backbone) or of amino
    acids with various side-chaines. "))



(defclass nucleotide-sequence (bio-sequence-record)
  ((circular :accessor circular-p
             :initarg :circular
             :initform nil
             :documentation "True iff the sequence is circular (O RLY?).")
   (alphabet :accessor alphabet
             :initarg :alphabet
             :initform nil
             :documentation "The sequence type. It's typically
             one of DNA or RNA, but it can specify other types as
             well.")
   (strand :accessor strand
           :initarg :strand
           :initform 0
           :type (integer -1 1)
	   :documentation "The direction in which the feature is found
           on the genome, if applicable. The number 1 denotes the
           direction from the small chromosomal arm (p like petit) to
           the larger (q). For bacterial chromosomes, it's a matter of
           convention which strand is named +1 and -1,
           respectively."))

  (:documentation "Nucleotide sequences can be retrieved from genomic
   databases (like Ensembl) which is implemented in CBCL. From
   Ensembl, nucleotides can also be retrieved as genes (with exons and
   introns) or transcripts. Specialised databases offer information on
   expressed sequence tags (ESTs)."))



(defclass amino-acid-sequence (bio-sequence-record) ()
  (:documentation "A real protein or at least some smallish peptide."))



(defun copy-bio-sequence (seq)
  "Return a fresh copy of the bio-sequence object."
  (moptilities:copy-template seq))


;;;; =========================================================================
;;;; Define interval compartible behavior of bio-sequences
;;;; =========================================================================
(defmethod make-interval ((bio-seq trivial-bio-sequence) lower upper &rest args)
  (declare (ignore args))
  (let ((new-seq (moptilities:copy-template bio-seq)))
    (setf (lower-number new-seq) lower
          (upper-number new-seq) upper)
    (when (not (null (bio-sequence-seq new-seq)))
        (setf (bio-sequence-seq new-seq)
              (subseq (bio-sequence-seq new-seq) (1- lower) upper)))
    new-seq))

;; (defmethod make-interval ((class #.(find-class 'trivial-bio-sequence))
;;                           lower upper &rest args)
;;   (apply #'make-instance class
;;          :seq-start lower
;;          :seq-end upper
;;          args))



(defgeneric bio-subseq (bio-sequence start &optional end)
  (:documentation "Return a slice of the bio-sequence from start to end.")
  (:method ((seq trivial-bio-sequence) (start integer)
            &optional (end (bio-sequence-length seq)))
    (let ((new-seq (make-interval seq start end)))
      (setf (bio-sequence-seq new-seq)
            (subseq (seq seq) (1- start) end))
      (setf (direct-superseq new-seq) seq)
      ;; Side effects... I'm not sure I like that.
      #+nil(push new-seq (direct-subseqs seq))
      new-seq)))


;; (defmethod print-object ((seq trivial-bio-sequence) stream)
;;   "The sequence object is printed to an output stream. This is fairly
;;   handy but somehow we feel that some more abstract and more generic
;;   mechanism is required."
;;   (print-unreadable-object (seq stream :type t)
;;     (flet ((print-slot-if-bound (slot-name)
;;            (when (slot-boundp seq slot-name)
;;              (format stream " ~(~A~): ~A" slot-name
;;                      (slot-value seq slot-name)))))
;;       (mapcar #'print-slot-if-bound '(id name)))))


(defgeneric bio-sequence-length (seq)
  (:documentation "Return the number of monomers in this sequence.")
  (:method ((seq trivial-bio-sequence)) (length (bio-sequence-seq seq))))


(defclass feature (integer-interval)
  ((feature-type :accessor feature-type
                 :initarg :feature-type))
  (:documentation "A property of interest that is referred to from a
  biological entity, i.e, a biological sequence."))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates on bio-sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric aa-coords->nt-coords (coord)
  (:documentation "Convert coordinates from amino acids to
   nucleotides.  This works for simple integers as well as for
   intervals.  Note that the first monomere has the index 1 in eather
   coordinate system, nucleotide as well as amino acid.  If the
   coordinate is a single number, it is mapped to the _first_
   nucleotide which codes for the corresponding amino acid. If the
   argument is an interval on an amino acid sequence, the result is an
   interval that spans the whole nucleotide sequence which codes for
   the corresponding amino acids.

   ### Examples
   (aa->nt 1)
   => 1
   (aa->nt 3)
   => 7 "))

(defmethod aa-coords->nt-coords ((coord integer))
  (1+ (* 3 (1- coord))))

(defmethod aa-coords->nt-coords ((interval integer-interval))
  (make-interval interval
                 (aa-coords->nt-coords (lower-bound interval))
                 (+ 2 (aa-coords->nt-coords (upper-bound interval)))))

(defgeneric nt-coords->aa-coords (coord)
  (:documentation "Convert from coordinates in nucleotides to
   coordinates in amino acids.

   Example:

   * (mapcar #'nt-coords->aa-coords (list 1 2 3 4 5 6 7 8 9))

   (1 1 1 2 2 2 3 3 3)"))

(defmethod nt-coords->aa-coords ((coord integer))
  (multiple-value-bind (aa-1 rest) (truncate (1- coord) 3)
    (values (1+ aa-1) rest)))

(defmethod nt-coords->aa-coords ((interval integer-interval))
  (bind:bind (((values start start-off)
               (nt-coords->aa-coords (lower-bound interval)))
              ((values end end-off)
               (nt-coords->aa-coords (upper-bound interval))))
    (values (make-interval interval  start end) start-off end-off)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sequence objects to closely model real world molecules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now those are very similar to ensembl objects.  I might change
;; that later.
(defclass transcript (nucleotide-sequence)
  ((exons :accessor exons
          :initarg :exons
          :initform #()
          :type sequence)
   (protein :accessor protein
            :initarg :protein
            :initarg :translation
            :documentation "The protein the transcript is coding for, if any."
            :initform nil))
   (:documentation "A transcript describes a RNA sequence that can be
    spliced into a mature mRNA.  Exons are the elements that make up
    the mRNA.  All sequence elements which are cut out in the splicing
    process are called introns and can be thought to be the relative
    complements of the exons within the transcript."))

(defclass exon (nucleotide-sequence)
  ((transcript :accessor transcript
               :initarg transcript
               :documentation "The transcript corresponding to this exon.")
   (circular :allocation :class
             :initform nil))
  (:documentation "Exons are those regions of the transcript of the
  genomic DNA that leave the nucleus and are read out to form the
  amino acid sequence. A single gene is very likely to have multiple
  variants that are assembled from different exons. The exonic regions
  from different transcript may overlap when mapped back onto the
  genome."))

(defclass protein (amino-acid-sequence)
  ((transcript :initarg :transcript
               :accessor transcript
               :documentation "The transcript which codes for this protein.")
   (features :initarg :features
             :accessor protein-features
             :documentation "Slot harboring the features of that
             protein sequence. This may be post-translational
             modifications or links to protein domain databases that
             are manifested by a respective sequence similarity at
             that particular region."))
  (:documentation "Proteins are the class of molecules that most
  biochemical functions are attributed to."))

(defclass protein-feature (feature)
  ((protein :initarg protein
            :accessor protein
            :accessor translation
            :documentation "The link back to the protein to which this
            feature belongs.")
   (feat-start :initarg :start
               :accessor feat-start
               :accessor lower-bound)
   (feat-end :initarg :end
             :accessor feat-end
             :accessor upper-bound))
  (:documentation "Whenever a fragment of a protein sequence is known
  to be special, then this class, the protein feature, is how this
  knowledge should be expressed formally. The name 'feature' is
  derived from the term 'feature-table' as it is used in the
  EMBL-formatted sequence databases like 'uniprot'. EnsEMBL and
  UniProt collaborate on protein annotation."))

(defclass transmembrane-helix (protein-feature)
  ((sidedness :initarg nil
	      :documentation "With N-terminus inside and C-terminus
	      outside, a membrane-spanning helix has an in-out
	      topology. The alternative is an out-in topology. The
	      default is nil. The sidedness is most difficult to
	      predict."))
  (:documentation "Membrane proteins are essential to understand for
  the pharmaceutical industry and they are fascinating in their own
  right. There are many things that one wants to know about
  membrane-spanning regions in proteins. Their sidedness is of
  concern, but also information about their aromatic ring or the
  distribution of stop-signals around them."))

