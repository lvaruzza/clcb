(in-package #'tinaa)
(tinaa:document-system :package 'cbcl
                       (pathname-directory *default-pathname-defaults*)
		       :write-files? t
		       :show-parts-without-documentation? nil
)
