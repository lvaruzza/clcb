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

(defclass bio-sequence (molecule integer-interval)
  ((id :accessor bio-sequence-id
       :initarg :id
       :initform ""
       :documentation "The primary ID of the sequence (genbank etc.)")
   (name :accessor bio-sequence-name
         :initarg :name
         :initform "Unknown"
         :documentation "The sequence's name.")
   (seq :accessor bio-sequence-seq
        :initarg :seq
        :initform nil
        :documentation "The actual sequence information. This can be
of any type, but will normally be of type `string'.")
   (description :accessor bio-sequence-description
                :initarg :description
                :documentation "The descripitions of the object.")
   (seq-start :accessor seq-start
              :accessor lower-bound
              :initarg :seq-start
              :initform 1)
   (seq-end :accessor seq-end
            :accessor upper-bound
            :initarg :seq-end))
   (:documentation "A biological sequence is a polymeric macromolecule
of nucelic acids (with a phorsphor-sugar backbone) or of amino acids
with various side-chaines. In daily routine, one does not use the full
genome but some fraction of it. And in this fraction we are interested
in smaller fractions that have (or are presumed to have) properties of
our interest. These regions, consecutive stretches within something
larger, may handily be understood as intervals and CLCB offers such an
interface to them."))


(defmethod initialize-instance :after ((seq bio-sequence) &rest args)
  (declare (ignore args))
  (unless (slot-boundp seq 'seq-end)
    (setf (seq-end seq) (length (bio-sequence-seq seq)))))


(defclass nucleotide-sequence (bio-sequence)
  ((circular :accessor circular-p
             :initarg :circular
             :initform nil
             :documentation "This is true iff the sequence is circular
                             (O RLY?)")
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
on the genome, if applicable. The number 1 denotes the direction from
the small chromosomal arm (p like petit) to the larger (q). For
bacterial chromosomes, it's a matter of convention which strand is
named +1 and -1, respectively."))
  (:documentation "Nucleotide sequences can be retrieved from genomic
databases (like Ensembl) which is implemented in CBCL. From Ensembl,
nucleotides can also be retrieved as genes (with exons and introns) or
transcripts. Specialised databases offer information on expressed
sequence tags (ESTs)."))

(defclass amino-acid-sequence (bio-sequence) ()
  (:documentation "A real protein or at least some smallish peptide."))

(defun copy-bio-sequence (seq)
  "Return a fresh copy of the bio-sequence object."
  ;; Copying all slot by themself is probably pretty stupid, but I
  ;; can't think of a better solution right now (Well, Gary King's MOP
  ;; based copy function would do => will implement)
  (moptilities:copy-template seq)
  #||(make-instance 'bio-sequence
                 :id (bio-sequence-id seq)
                 :name (bio-sequence-name seq)
                 :seq (bio-sequence-seq seq)
                 :mol-weight (mol-weight seq))||#
)

(defmethod make-interval ((bio-seq bio-sequence) lower upper &rest args)
  (declare (ignore args))
  (let ((new-seq (moptilities:copy-template bio-seq)))
    (setf (lower-bound new-seq) lower
          (upper-bound new-seq) upper)
    (when (not (null (bio-sequence-seq new-seq)))
        (setf (bio-sequence-seq new-seq)
              (subseq (bio-sequence-seq new-seq) (1- lower) upper)))
    new-seq))

(defmethod make-interval ((class (eql (find-class 'nucleotide-sequence)))
                          lower upper &rest args)
  (make-instance class
                 :seq-start lower
                 :seq-end upper))


(defmethod print-object ((seq bio-sequence) stream)
  "The sequence object is printed to an output stream. This is fairly
handy but somehow we feel that some more abstract and more generic
mechanism is required."  
  (print-unreadable-object (seq stream :type t)
    (flet ((print-slot-if-bound (slot-name)
           (when (slot-boundp seq slot-name)
             (format stream " ~(~A~): ~A" slot-name
                     (slot-value seq slot-name)))))
      (mapcar #'print-slot-if-bound '(id name)))))


(defgeneric bio-sequence-length (seq)
  (:documentation "Return the number of monomers in this sequence.")
  (:method ((seq bio-sequence)) (length (bio-sequence-seq seq))))


(defclass feature (integer-interval)
  ((feature-type :accessor feature-type
                 :initarg :feature-type))
  (:documentation "A property of interest that is referred to from a
biological entity, i.e, a biological sequence."))

(defgeneric shuffle-sequence (seq)
  (:documentation "Return a randomly shuffled copy of the sequence." )
  (:method ((seq bio-sequence)) 
    (let ((shuffled (copy-bio-sequence seq)))
      (setf (bio-sequence-seq shuffled)
            (nshuffle-vector (bio-sequence-seq shuffled)))
      shuffled)))


(defun orthogonal-coded (char)
  "Return the character as an orthogonal coded vector.  Vectors are
orthogonal iff their scalar product equals zero. The scalar product of
concatenations of othogonal coded bases gives the number of
nucleotides that the sequences have in common.

http://en.wikipedia.org/wiki/Orthogonal_array
"
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sequence objects to closely model real world molecules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now those are very similar to ensembl objects. maybe I will
;; change that later.
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
   (:documentation "An transcript describes a RNA sequence that can be
spliced into a mature mRNA.  Exons are the elements that make up the
mRNA.  All sequence elements which are cut out in the splicing process
are called introns and can be thought to be the relative complements of
the exons within the transcript."))

(defclass exon (nucleotide-sequence)
  ((transcript :accessor transcript
               :initarg transcript
               :documentation "The transcript corresponding to this exon.")
   (circular :allocation :class
             :initform nil)))

(defclass protein (amino-acid-sequence)
  ((transcript :initarg :transcript
               :accessor transcript
               :documentation "The transcript which codes for this protein.")
   (features :initarg :features
             :accessor protein-features
             :documentation "Protein features")))

(defclass protein-feature (feature)
  ((protein :initarg protein
            :accessor protein
            :accessor translation
            :documentation "the protein to which this feature belongs.")
   (feat-start :initarg :start
               :accessor feat-start
               :accessor lower-bound)
   (feat-end :initarg :end
             :accessor feat-end
             :accessor upper-bound)))

(defclass transmembrane-helix (protein-feature)
  ())

