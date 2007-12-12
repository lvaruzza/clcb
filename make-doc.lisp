;(in-package :clcb-ensembl)
;(tinaa:document-system :package *package*
;                       "html/"
;		       :write-files? t
;		       :show-parts-without-documentation? nil
;)
;(in-package :clcb-utils)
;(tinaa:document-system :package *package*
;                       "html/"
;		       :write-files? t
;		       :show-parts-without-documentation? nil
;)
;(in-package :clcb)
;(tinaa:document-system :package *package*
;                       "html/"
;		       :write-files? t
;		       :show-parts-without-documentation? nil
;)

(tinaa:document-system :asdf-system 'clcb
                       "html/" )

;                       (pathname-directory *default-pathname-defaults*)

