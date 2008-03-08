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

(let ((counter 0))
  (defun bio-sequence-counter ()
    (incf counter))
  (defun reset-bio-sequence-counter ()
    (setf counter 0)))


(defmacro define-make-instance-bio-sequence (bio-sequence-name
                                             trivial-bio-sequence-name
                                             fragmented-bio-sequence-name)
  "Create an interface to make-instance.  This enforces each object of
  type `bio-sequence-name' to be either of type
  `trivial-${bio-sequence-name}' or `fragmented-${bio-sequence-name}'.
  Un-subclassed instances are impossible."
  `(progn
     (defmethod make-instance ((class (eql (find-class ',bio-sequence-name)))
                               &rest initargs &key &allow-other-keys)
       (declare (ignore class))
       (let ((fragmented-seq-p (some #'(lambda (key)
                                         (getf initargs key))
                                     '(:seq-fragments :intervals))))
         (apply #'make-instance (if fragmented-seq-p
                                    ',fragmented-bio-sequence-name
                                   ',trivial-bio-sequence-name)
                initargs)))))
;;;; =========================================================================
;;;; Symbol utilities
;;;; =========================================================================
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bio-sequence-types* 
    `((:generic .    ,(string ""))
      (:trivial .    ,(string '#:trivial-))
      (:fragmented . ,(string '#:fragmented-))))
  (defun %bio-sequence-types ()
    (mapcar #'car *bio-sequence-types*))
  (defun %bio-sequence-class-prefixes ()
    (mapcar #'cdr *bio-sequence-types*))
  (defun %bio-sequence-class-prefix (type)
    (cdr (assoc type *bio-sequence-types*))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concatenate-symbols (&rest symbols)
    (intern (apply #'concatenate 'string
                   (mapcar #'string symbols))))


  (defun find-bio-sequence-class (class &optional (type :generic) (errorp t))
    "Return the bio-sequence of type `type' for `class'."
    ;; (declare (type (member #.(%bio-sequence-types) )))
    (if (typep class 'standard-class)
        (find-bio-sequence-class (class-name class) type)
        (let ((class-name 
               (iter
                 (with cls-string = (string class))
                 (for type-prefix in (%bio-sequence-class-prefixes))
                 (for search-res = (search type-prefix cls-string
                                           :test #'string-equal))
                 (when (and search-res (zerop search-res))
                   (setf cls-string (subseq cls-string (length type-prefix))))
                 (finally (return cls-string)))))
         (find-class (concatenate-symbols (%bio-sequence-class-prefix type)
                                          class-name)
                     errorp))))
  

  (defun replace-bio-superclasses (superclasses type)
    "Replace elements of `superclasses' with a ${type}-bio-sequence if
    possible."
    (let ((replace-occured nil))
      (let ((new-superclasses
             (mapcar #'(lambda (superclass)
                         (let ((superbio 
                                (find-bio-sequence-class superclass type nil)))
                           (if superbio
                               (progn (setf replace-occured t)
                                      (class-name superbio))
                               superclass)))
                     superclasses)))
        (values new-superclasses replace-occured)))))

(defmacro define-bio-sequence (bio-sequence-name superclasses
                               &body slots-and-docs)
  "Define a new bio-sequence class.  Parameters are the same as for a
  normal defclass construct."
  (flet ((prefixed-class (prefix)
           (concatenate-symbols prefix bio-sequence-name))
         (class-symbol (class)
           (typecase class
             (symbol class)
             (standard-class (class-name class))
             (standard-object (class-name (class-of class)))))
         (superclasses (type)
           (bind (((:values super found-one)
                   (replace-bio-superclasses superclasses type)))
             (if found-one
                 super
                 (append super (list (find-bio-sequence-class 'bio-sequence
                                                              type)))))))
    (let*
        ((export? (not (cdr (getf slots-and-docs :export))))
         (trivial-bio-sequence-name    (prefixed-class '#:trivial-))
         (fragmented-bio-sequence-name (prefixed-class '#:fragmented-))
         (trivial-superclasses         (mapcar #'class-symbol
                                               (superclasses :trivial)))
         (fragmented-superclasses      (mapcar #'class-symbol
                                               (superclasses :fragmented))))
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (defclass ,bio-sequence-name
            ,(if (some #'(lambda (x) (subtypep x 'abstract-bio-sequence))
                       superclasses)
                 superclasses
                 (append superclasses (list 'abstract-bio-sequence)))
          ,@slots-and-docs)
        (defclass ,trivial-bio-sequence-name
            (,bio-sequence-name ,@trivial-superclasses)
          ())
        (defclass ,fragmented-bio-sequence-name
            (,bio-sequence-name ,@fragmented-superclasses)
          ())

        ;; Export if needed
        ,(when export?
               `(export '(,bio-sequence-name
                          ,trivial-bio-sequence-name
                          ,fragmented-bio-sequence-name)))

        (define-make-instance-bio-sequence
            ,bio-sequence-name
            ,trivial-bio-sequence-name
          ,fragmented-bio-sequence-name)
        ;; Provide a useful return value (not necessary, but might be
        ;; usefull in some cases (and it doesn't hurt, either).
        (find-class ',bio-sequence-name)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
 (defclass abstract-bio-sequence (molecule abstract-interval)
   ((id
     :accessor bio-sequence-id
     :initarg :id
     :initform (format nil "unknown-seq-~d" (bio-sequence-counter))
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


 (defclass fragmented-bio-sequence (abstract-bio-sequence
                                    abstract-multi-interval)
   ((seq-fragments
     :accessor seq-fragments
     :accessor intervals  ; For compliance with the interval protokoll
     :initarg :seq-fragments
     :initarg :intervals
     :type sequence
     :initform nil
     :documentation "The fragments of this sequence, each one being a
    bio-sequence itself."))
   (:documentation "A bio sequence which is constructed from two or
  more elementary sequences.  A well known examples for this would be
  a spliced mRNA, which is constituted by a number of exons."))



 (define-bio-sequence bio-sequence-record ()
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



 (define-bio-sequence nucleotide-sequence (bio-sequence-record)
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


 (define-bio-sequence amino-acid-sequence (bio-sequence-record)
   ()
   (:documentation "A real protein or at least some smallish peptide.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sequence objects to closely model real world molecules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now those are very similar to ensembl objects.  I might change
;; that later.

(define-bio-sequence transcript (nucleotide-sequence)
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


(define-bio-sequence exon (nucleotide-sequence)
  ((transcript :accessor transcript
               :initarg transcript
               :documentation "The transcript corresponding to this exon.")
   (circular :allocation :class
             :initform nil))
  (:documentation "Exons are those regions of the transcript of the
   genomic DNA that leave the nucleus and are read out to form the
   amino acid sequence. A single gene is very likely to have multiple
   variants that are assembled from different exons. The exonic
   regions from different transcript may overlap when mapped back
   onto the genome."))

(define-bio-sequence protein (amino-acid-sequence)
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

(define-bio-sequence protein-feature (feature)
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

(define-bio-sequence transmembrane-helix (protein-feature)
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

;;;; END: BIO-SEQUENCE CLASSES
;;;; =========================================================================


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

(defmethod print-object ((fragseq fragmented-bio-sequence) (stream stream))
  (print-unreadable-object (fragseq stream :type t :identity nil)
    (format stream "\"~A\" (~{~A~^ ~})"
            (bio-sequence-id fragseq)
            (mapcar #'(lambda (_) 
                        (format nil "#[~d,~d]"
                                (lower-number _)
                                (upper-number _)))
                    (seq-fragments fragseq)))))

(defun super-sequence-root-p (bio-sequence)
  "Return true iff `bio-sequence' is the topmost node in a tree of
  subsequences -- this is the case exactly when there exists no other
  bio-sequence object to which bio-sequence is a sub-sequence."
  (with-accessors ((ds direct-superseq)) bio-sequence
    (or (null ds)
        (eq (direct-superseq ds) ds)
        (eq ds t))))

(defun super-sequences (bio-sequence)
  "Return a list of super-sequences, where each list element is the
  superseq of is predecessor (if any)."
  (declare (type abstract-bio-sequence bio-sequence))
  (let ((direct-superseq (direct-superseq bio-sequence)))
    (if (or (eq (direct-superseq direct-superseq) direct-superseq)
            (null direct-superseq))
        (list direct-superseq)
        (cons direct-superseq (super-sequences direct-superseq)))))





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


(defgeneric bio-subseq (bio-sequence start-or-interval &optional end)
  (:documentation "Return a slice of the bio-sequence from start to end.")

  (:method ((bioseq trivial-bio-sequence) (start integer) 
            &optional (end (bio-sequence-length bioseq)))
    (let ((new-seq (make-interval bioseq start end)))
      (setf (bio-sequence-seq new-seq)
            (subseq (seq bioseq) (1- start) end))
      (setf (direct-superseq new-seq) bioseq)
      ;; Side effects... I'm not sure I like that.
      #+nil(push new-seq (direct-subseqs bioseq))
      new-seq))

  (:method ((bioseq trivial-bio-sequence) (interval integer-interval)
            &optional end)
    (declare (ignore end))
    (bio-subseq bioseq (lower-number interval) (upper-number interval)))

  (:method ((bioseq trivial-bio-sequence) (multint abstract-multi-interval)
            &optional end)
    (declare (ignore end))
    (let ((new-seq (make-instance
                    (find-bio-sequence-class (class-of bioseq) :fragmented)
                    :seq-fragments (mapcar (curry #'bio-subseq bioseq)
                                           (intervals multint)))))
      (setf (direct-superseq new-seq) bioseq)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinates on bio-sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline %aa->nt))
(defun %aa->nt (integer)
  (declare (integer integer))
  (1+ (* 3 (1- integer))))

(defgeneric aa-coords->nt-coords (coordinate)
  (:documentation "Convert coordinates from amino acids to
   nucleotides.  This works for simple integers as well as for
   intervals.  Note that the first monomere has the index 1 in either
   coordinate system, nucleotide as well as amino acid.  This methods
   returns an interval spanning the whole nucleotide sequence which
   codes for the corresponding amino acid.  If an integer `i' is
   given, it is interpreted as the interval #[i,i].

   ### Examples
   (aa-coords->nt-coords 1)
   => 1
   (aa->nt 3)
   => 7 "))

(defmethod aa-coords->nt-coords ((coord integer))
  (let ((start (%aa->nt coord)))
    (make-interval 'integer-interval start (+ 2 start))))

(defmethod aa-coords->nt-coords ((interval integer-interval))
  (make-interval interval
                 (%aa->nt (lower-number interval))
                 (+ 2 (%aa->nt (upper-number interval)))))


(declaim (inline %nt->aa))
(defun %nt->aa (integer)
  (declare (integer integer))
  (multiple-value-bind (aa-minus-1 rest) (truncate (1- integer) 3)
    (values (1+ aa-minus-1) rest)))

(defgeneric nt-coords->aa-coords (nt-coordinate)
  (:documentation "Convert from coordinates in nucleotides to
   coordinates in amino acids.  Thereby, an interval of nucleotides is
   mapped to the interval of _all_ amino acids which are at least
   partly coded by this sequence.  If a single integer is given, a one
   elemental interval is returned."))

(defmethod nt-coords->aa-coords ((coord integer))
  (bind (((:values aa-coord rest) (%nt->aa coord)))
    (values (make-interval 'integer-interval aa-coord aa-coord )
            rest)))

(defmethod nt-coords->aa-coords ((interval integer-interval))
  (bind (((:values start start-off)
          (%nt->aa (lower-number interval)))
         ((:values end end-off)
          (%nt->aa (upper-number interval))))
    (values (make-interval interval  start end) start-off end-off)))



