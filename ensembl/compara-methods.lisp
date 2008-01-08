;;;; Copyright (c) 2007 Steffen Moeller
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

;; The aim of these classes is mostly to add links between ortholog genes.

;; should be an ensembl-object, probably ?

(in-package :clcb-ensembl)

#||

homology

+----------------------------+------------------+------+-----+---------+----------------+
| Field                      | Type             | Null | Key | Default | Extra          |
+----------------------------+------------------+------+-----+---------+----------------+
| homology_id                | int(10) unsigned |      | PRI | NULL    | auto_increment |
| stable_id                  | varchar(40)      | YES  |     | NULL    |                |
| method_link_species_set_id | int(10) unsigned |      | MUL | 0       |                |
| description                | varchar(40)      | YES  |     | NULL    |                |
| subtype                    | varchar(40)      |      |     |         |                |
| dn                         | float(10,5)      | YES  |     | NULL    |                |
| ds                         | float(10,5)      | YES  |     | NULL    |                |
| n                          | float(10,1)      | YES  |     | NULL    |                |
| s                          | float(10,1)      | YES  |     | NULL    |                |
| lnl                        | float(10,3)      | YES  |     | NULL    |                |
| threshold_on_ds            | float(10,5)      | YES  |     | NULL    |                |
| ancestor_node_id           | int(10) unsigned |      |     | 0       |                |
| tree_node_id               | int(10) unsigned |      |     | 0       |                |
+----------------------------+------------------+------+-----+---------+----------------+

mysql> select homology_id,stable_id,method_link_species_set_id,description,subtype from homology limit 10;
+-------------+-----------+----------------------------+------------------------+-------------------------+
| homology_id | stable_id | method_link_species_set_id | description            | subtype                 |
+-------------+-----------+----------------------------+------------------------+-------------------------+
|           1 | NULL      |                      20176 | within_species_paralog | Ciona intestinalis      |
|           2 | NULL      |                      20176 | within_species_paralog | Ciona intestinalis      |
|           3 | NULL      |                      21191 | within_species_paralog | Drosophila melanogaster |
|           4 | NULL      |                      20059 | ortholog_one2one       | Tetraodontidae          |
|           5 | NULL      |                      20521 | ortholog_one2one       | Culicidae               |
|           6 | NULL      |                      20834 | ortholog_one2one       | Homo/Pan/Gorilla group  |
|           7 | NULL      |                      20059 | ortholog_one2one       | Tetraodontidae          |
|           8 | NULL      |                      21246 | ortholog_one2one       | Diptera                 |
|           9 | NULL      |                      20453 | ortholog_one2one       | Ciona                   |
|          10 | NULL      |                      20453 | ortholog_one2one       | Ciona                   |
+-------------+-----------+----------------------------+------------------------+-------------------------+
10 rows in set (0,05 sec)

homology_member
+--------------------------+------------------+------+-----+---------+-------+
| Field                    | Type             | Null | Key | Default | Extra |
+--------------------------+------------------+------+-----+---------+-------+
| homology_id              | int(10) unsigned |      | PRI | 0       |       |
| member_id                | int(10) unsigned |      | PRI | 0       |       |
| peptide_member_id        | int(10) unsigned | YES  | MUL | NULL    |       |
| peptide_align_feature_id | int(10) unsigned | YES  | MUL | NULL    |       |
| cigar_line               | mediumtext       | YES  |     | NULL    |       |
| cigar_start              | int(10)          | YES  |     | NULL    |       |
| cigar_end                | int(10)          | YES  |     | NULL    |       |
| perc_cov                 | int(10)          | YES  |     | NULL    |       |
| perc_id                  | int(10)          | YES  |     | NULL    |       |
| perc_pos                 | int(10)          | YES  |     | NULL    |       |
+--------------------------+------------------+------+-----+---------+-------+
10 rows in set (0.04 sec)


member

+----------------+------------------+------+-----+---------+----------------+
| Field          | Type             | Null | Key | Default | Extra          |
+----------------+------------------+------+-----+---------+----------------+
| member_id      | int(10) unsigned |      | PRI | NULL    | auto_increment |
| stable_id      | varchar(40)      |      | MUL |         |                |
| version        | int(10)          | YES  |     | 0       |                |
| source_name    | varchar(40)      |      | MUL |         |                |
| taxon_id       | int(10) unsigned |      | MUL | 0       |                |
| genome_db_id   | int(10) unsigned | YES  | MUL | NULL    |                |
| sequence_id    | int(10) unsigned | YES  | MUL | NULL    |                |
| gene_member_id | int(10) unsigned | YES  | MUL | NULL    |                |
| description    | text             | YES  |     | NULL    |                |
| chr_name       | varchar(40)      | YES  |     | NULL    |                |
| chr_start      | int(10)          | YES  |     | NULL    |                |
| chr_end        | int(10)          | YES  |     | NULL    |                |
| chr_strand     | tinyint(1)       |      |     | 0       |                |
| display_label  | varchar(128)     | YES  |     | NULL    |                |
+----------------+------------------+------+-----+---------+----------------+

mysql> desc method_link_species_set;
+----------------------------+------------------+------+-----+---------+----------------+
| Field                      | Type             | Null | Key | Default | Extra          |
+----------------------------+------------------+------+-----+---------+----------------+
| method_link_species_set_id | int(10) unsigned |      | PRI | NULL    | auto_increment |
| method_link_id             | int(10) unsigned | YES  | MUL | NULL    |                |
| species_set_id             | int(10) unsigned |      |     | 0       |                |
| name                       | varchar(255)     |      |     |         |                |
| source                     | varchar(255)     |      |     | ensembl |                |
| url                        | varchar(255)     |      |     |         |                |
+----------------------------+------------------+------+-----+---------+----------------+
6 rows in set (0.05 sec)


mysql> select * from genome_db limit 30;
+--------------+----------+-------------------------------+------------+------------------+--------------------+---------+
| genome_db_id | taxon_id | name                          | assembly   | assembly_default | genebuild          | locator |
+--------------+----------+-------------------------------+------------+------------------+--------------------+---------+
|            3 |    10116 | Rattus norvegicus             | RGSC3.4    |                1 | 2006-02-Ensembl    |         |
|            4 |    31033 | Takifugu rubripes             | FUGU4      |                1 | 2005-05-IMCB       |         |
|            5 |     7165 | Anopheles gambiae             | AgamP3     |                1 | 2007-06-VectorBase |         |
|           13 |    99883 | Tetraodon nigroviridis        | TETRAODON7 |                1 | 2004-09-Genoscope  |         |
|           16 |     8364 | Xenopus tropicalis            | JGI4.1     |                1 | 2005-11-Ensembl    |         |
|           18 |     7719 | Ciona intestinalis            | JGI2       |                1 | 2006-02-Ensembl    |         |
|           22 |     9606 | Homo sapiens                  | NCBI36     |                1 | 2006-12-Ensembl    |         |
|           27 |    51511 | Ciona savignyi                | CSAV2.0    |                1 | 2006-04-Ensembl    |         |
|           29 |     7159 | Aedes aegypti                 | AaegL1     |                1 | 2006-06-VectorBase |         |
|           31 |     9544 | Macaca mulatta                | MMUL_1     |                1 | 2006-01-Ensembl    |         |
|           32 |     9785 | Loxodonta africana            | BROADE1    |                1 | 2006-08-Ensembl    |         |
|           33 |     9371 | Echinops telfairi             | TENREC     |                1 | 2006-08-Ensembl    |         |
|           34 |     9986 | Oryctolagus cuniculus         | RABBIT     |                1 | 2006-08-Ensembl    |         |
|           35 |     9361 | Dasypus novemcinctus          | ARMA       |                1 | 2006-08-Ensembl    |         |
|           36 |    69293 | Gasterosteus aculeatus        | BROADS1    |                1 | 2006-06-Ensembl    |         |
|           37 |     8090 | Oryzias latipes               | MEDAKA1    |                1 | 2006-05-Ensembl    |         |
|           38 |     9598 | Pan troglodytes               | CHIMP2.1   |                1 | 2007-05-Ensembl    |         |
|           39 |     9615 | Canis familiaris              | BROADD2    |                1 | 2006-12-Ensembl    |         |
|           40 |     7227 | Drosophila melanogaster       | BDGP4.3    |                1 | 2006-03-FlyBase    |         |
|           42 |     9031 | Gallus gallus                 | WASHUC2    |                1 | 2006-08-Ensembl    |         |
|           43 |     9258 | Ornithorhynchus anatinus      | OANA5      |                1 | 2007-01-Ensembl    |         |
|           44 |     4932 | Saccharomyces cerevisiae      | SGD1.01    |                1 | 2006-12-SGD        |         |
|           45 |     9913 | Bos taurus                    | Btau_3.1   |                1 | 2006-09-Ensembl    |         |
|           46 |    13616 | Monodelphis domestica         | BROADO5    |                1 | 2007-02-Ensembl    |         |
|           47 |     9685 | Felis catus                   | CAT        |                1 | 2006-06-Ensembl    |         |
|           48 |    37347 | Tupaia belangeri              | TREESHREW  |                1 | 2006-10-Ensembl    |         |
|           49 |     9365 | Erinaceus europaeus           | HEDGEHOG   |                1 | 2006-10-Ensembl    |         |
|           50 |    10141 | Cavia porcellus               | GUINEAPIG  |                1 | 2006-10-Ensembl    |         |
|           51 |    30611 | Otolemur garnettii            | BUSHBABY1  |                1 | 2007-02-Ensembl    |         |
|           52 |    43179 | Spermophilus tridecemlineatus | SQUIRREL   |                1 | 2006-10-Ensembl    |         |
+--------------+----------+-------------------------------+------------+------------------+--------------------+---------+

mysql> select distinct source_name from member;
+-------------------+
| source_name       |
+-------------------+
| ENSEMBLGENE       |
| ENSEMBLPEP        |
| Uniprot/SPTREMBL  |
| Uniprot/SWISSPROT |
+-------------------+
4 rows in set (2.40 sec)

mysql> select member_id,stable_id,source_name,taxon_id,genome_db_id,sequence_id,gene_member_id,chr_name from member limit 10;
+-----------+--------------------+-------------+----------+--------------+-------------+----------------+-----------+
| member_id | stable_id          | source_name | taxon_id | genome_db_id | sequence_id | gene_member_id | chr_name  |
+-----------+--------------------+-------------+----------+--------------+-------------+----------------+-----------+
|         1 | ENSG00000215614    | ENSEMBLGENE |     9606 |           22 |        NULL |           NULL | NT_113923 |
|         2 | ENSP00000383514    | ENSEMBLPEP  |     9606 |           22 |           1 |              1 | NT_113923 |
|         3 | ENSG00000212857    | ENSEMBLGENE |     9606 |           22 |        NULL |           NULL | NT_113923 |
|         4 | ENSP00000375387    | ENSEMBLPEP  |     9606 |           22 |           2 |              3 | NT_113923 |
|         5 | ENSG00000215610    | ENSEMBLGENE |     9606 |           22 |        NULL |           NULL | NT_113923 |
|         6 | ENSP00000341691    | ENSEMBLPEP  |     9606 |           22 |           3 |              5 | NT_113923 |
|         7 | ENSMUSG00000078424 | ENSEMBLGENE |    10090 |           57 |        NULL |           NULL | NT_166433 |
|         8 | ENSMUSP00000100854 | ENSEMBLPEP  |    10090 |           57 |           4 |              7 | NT_166433 |
|         9 | R0010W             | ENSEMBLGENE |     4932 |           44 |        NULL |           NULL | 2-micron  |
|        10 | R0010W             | ENSEMBLPEP  |     4932 |           44 |           5 |              9 | 2-micron  |
+-----------+--------------------+-------------+----------+--------------+-------------+----------------+-----------+
10 rows in set (0.05 sec)

select * from 
 homology left join homology_member using(homology_id) left join member using (member_id) left join method_link_species_set on (homology.method_link_species_set_id=method_link_species_set.method_link_species_set_id) left join species_set using (species_set_id) left join genome_db using (genome_db_id) where homology.description='ortholog_one2one' and genome_db.name='Homo sapiens' and member.source_name='ENSEMBLGENE' limit 30;

||#


(defclass similarity ()
  ((id :type integer
       :accessor similarity-id
       :documentation "The ID of the Ensembl MySQL database entry in
  the respective table."))
  (:documentation "The compara DB defines multiple kinds of similarity
  that all share a lot in their formal representation. This common
  superclass aims at defining some common routines for them all but as
  a start is only a reminder to perform proper abstraction."))

(defclass domain (similarity)
  ()
  (:documentation "Multiple genes are said to share a domain when
  these have a strong local similarity in their sequences."))

(def-view-class homology ()
  ((homology-id :db-type :key
                         :type integer)
   (stable-id :type (string 40))
   (description :type (string 40)))
  (:base-table "homology")
  (:documentation "Orthologue or paralogue sequences are grouped in
  one abstract specification. It can be understood as an abstract
  property that several genes share."))

(def-view-class homology-member ()
  ((homology-id :db-kind :key
                :type integer)
   (member-id :db-kind :key
              :type integer)
   (homology :db-kind :join
             :db-info (:join-class homology
                       :home-key homology-id
                       :foreign-key homology-id)
             :accessor homology))
  (:base-table "homology_member"))

(def-view-class member-view ()
  ((member-id :db-kind :key
              :type integer)
   (stable-id :type (string 40))
   (homology :db-kind :join
             :db-info (:join-class homology-member
                       :home-key member-id
                       :foreign-key member-id)))
   (family   :db-kind :join
             :db-info (:join-class family-member
                       :home-key member-id
                       :foreign-key member-id)))
  (:base-table "member"))

(defmethod gene ((m member-view)) 
  "The member is practially a gene but the information is stored in different databases of the Ensembl resource."
  (fetch-by-stable-id (stable-id m)))

#||
| domain_id                  | int(10) unsigned |      | PRI | NULL    | auto_increment |
| stable_id                  | varchar(40)      |      | MUL |         |                |
| method_link_species_set_id | int(10) unsigned |      |     | 0       |                |
| description                | varchar(255)     | YES  |     | NULL    |                |
||#

(def-view-class domain ()
  ((domain_id :db-kind :key :type integer)
   (stable_id :db-kind :key :type (string 40))
   (description :db-kind :key :type (string 255)))
   (:base-table "domain")
   (:documentation "A domain is defined on protein sequences and commonly associated with a particular protein function."))

#||
| domain_id    | int(10) unsigned |      | MUL | 0       |       |
| member_id    | int(10) unsigned |      | MUL | 0       |       |
| member_start | int(10)          | YES  |     | NULL    |       |
| member_end   | int(10)          | YES  |     | NULL    |       |
||#
(def-view-class domain-member ()
  ((domain-id :db-kind :key :type integer)
   (member-id :db-kind :key :type integer)
   (member-start :type integer)
   (member-end :type integer))
  (:base-table "domain_member")
  (:documentation "Link between the domain and the gene ... or the peptide? The database is yet empty."))


#||
| family_id                  | int(10) unsigned |      | PRI | NULL    | auto_increment |
| stable_id                  | varchar(40)      |      | UNI |         |                |
| method_link_species_set_id | int(10) unsigned |      | MUL | 0       |                |
| description                | varchar(255)     | YES  | MUL | NULL    |                |
| description_score          | double           | YES  |     | NULL    |                |
||#

(def-view-class family ()
  ((family-id :db-kind :key :type integer)
   (stable-id :db-kind :key :type (string 40))
   (description :type (string 255))
   (description-score :type float))
  (:base-table "family")
  (:documentation "Ensembl gathers genes of similar function to protein families."))

#||
| family_id  | int(10) unsigned |      | PRI | 0       |       |
| member_id  | int(10) unsigned |      | PRI | 0       |       |
| cigar_line | mediumtext       | YES  |     | NULL    |       |
||#
(def-view-class family-member ()
  ((family-id :db-kind :key :type integer)
   (member-id :db-kind :key :type integer))
   (family :db-kind :join
             :db-info (:join-class family
                       :home-key family-id
                       :foreign-key family-id)
             :accessor family))
  (:base-table "family_member")
  (:documentation "Link between protein family and gene, represented in the member table."))


#||
| method_link_species_set_id | int(10) unsigned |      | PRI | NULL    | auto_increment |
| method_link_id             | int(10) unsigned | YES  | MUL | NULL    |                |
| species_set_id             | int(10) unsigned |      |     | 0       |                |
| name                       | varchar(255)     |      |     |         |                |
| source                     | varchar(255)     |      |     | ensembl |                |
| url                        | varchar(255)     |      |     |         |                |
||#
(def-view-class method-link-species-set ()
  ((method_link_species_set_id :db-kind :key :type integer)
   (method_link_id :type integer)
   (species_set_id :type integer)
   (name   :type (string 255))
   (source :type (string 255))
   (url    :type (string 255))
   )
  (:base-table "method_link_species_set")
  (:documentation "The assignment of a gene to a homology (in homology_member) depends on the species and the method that was applied. This class bridges the homology, not the assignment iself, with this additional information."))

#||
| method_link_id | int(10) unsigned |      | PRI | NULL    | auto_increment |
| type           | varchar(50)      |      | MUL |         |                |
| class          | varchar(50)      |      |     |         |                |
||#

#||
mysql> select * from method_link;
+----------------+--------------------------+---------------------------------------+
| method_link_id | type                     | class                                 |
+----------------+--------------------------+---------------------------------------+
|              1 | BLASTZ_NET               | GenomicAlignBlock.pairwise_alignment  |
|              2 | BLASTZ_NET_TIGHT         | GenomicAlignBlock.pairwise_alignment  |
|              3 | BLASTZ_RECIP_NET         | GenomicAlignBlock.pairwise_alignment  |
|              4 | PHUSION_BLASTN           | GenomicAlignBlock.pairwise_alignment  |
|              5 | PHUSION_BLASTN_TIGHT     | GenomicAlignBlock.pairwise_alignment  |
|              6 | TRANSLATED_BLAT          | GenomicAlignBlock.pairwise_alignment  |
|              7 | BLASTZ_GROUP             | GenomicAlignBlock.pairwise_alignment  |
|              8 | BLASTZ_GROUP_TIGHT       | GenomicAlignBlock.pairwise_alignment  |
|            101 | SYNTENY                  | SyntenyRegion.synteny                 |
|            201 | ENSEMBL_ORTHOLOGUES      | Homology.homology                     |
|            202 | ENSEMBL_PARALOGUES       | Homology.homology                     |
|            301 | FAMILY                   | Family.family                         |
|              9 | MLAGAN                   | GenomicAlignBlock.multiple_alignment  |
|            401 | PROTEIN_TREES            | ProteinTree.protein_tree_node         |
|            204 | ENSEMBL_HOMOLOGUES       | Homology.homology                     |
|             10 | PECAN                    | GenomicAlignBlock.multiple_alignment  |
|             11 | GERP_CONSTRAINED_ELEMENT | GenomicAlignBlock.constrained_element |
|            501 | GERP_CONSERVATION_SCORE  | ConservationScore.conservation_score  |
+----------------+--------------------------+---------------------------------------+
||#

(def-view-class method-link ()
  ((method-link-id :db-kind :key :type integer)
   (type :type (string 50))
   (class :type (string 50)))
  (:base-table "method_link")
  (:documentation "This table lists several handfull of approaches to determine sequence similarities. It is referred to from the method-link-species-set table which in turn may be linked to from all the relationships that are describing sequence similarities of some sort."))

#||
| species_set_id | int(10) unsigned |      | PRI | NULL    | auto_increment |
| genome_db_id   | int(10) unsigned |      | PRI | 0       |                |
||#

(def-view-class species-set ()
  ((species-set-id :db-kind :key :type integer)
   (genome-db-id   :db-kind :key :type integer))
  (:base-table "species_set")
  (:documentation "A certain similarity may be defined for a range of species. This table implements the n:m relationship between homology and species."))


#||
| genome_db_id     | int(10) unsigned |      | PRI | NULL    | auto_increment |
| taxon_id         | int(10) unsigned |      | MUL | 0       |                |
| name             | varchar(40)      |      | MUL |         |                |
| assembly         | varchar(100)     |      |     |         |                |
| assembly_default | tinyint(1)       | YES  |     | 1       |                |
| genebuild        | varchar(100)     |      |     |         |                |
| locator          | varchar(255)     | YES  |     | NULL    |                |
||#

(def-view-class genome-db ()
  ((genome-db-id :db-kind :key :type integer)
   (taxon-id     :db-kind :key :type integer)
   (name         :db-kind :key :type (string 40))
   (assembly     :db-kind :key :type (string 100))
   (genebuild    :db-kind :key :type (string 100))
   (locator      :db-kind :key :type (string 255)))
  (:base-table "genome_db")
  (:documentation "The genome_db table identifies species, though there may be multiple genomes per species, i.e. for the the human."))



;;; The following first attempts are no longer required

#||
(def-view-class orthologue (homologue) () (:documentation "Orthologues ... not yet implemented."))

(defclass paralogue (homologue) () (:documentation "Paralogues ... not yet implemented."))

(defmethod print-object ((o similarity) (s stream))
  (print-unreadable-object (o s :type t :identity nil)
          (format s "~A" (similarity-id o))))

(defmethod retrieve-genes ((o similarity))
  (print "Not yet implemented."))


(defun gene-stable-id->homology-ids (gene-stable-id &key )
  "Retrieval of set of homolog IDs that a gene is involved in."
  (let ((query (concatenate 'string "select homology_id from homology "
			  	  "right join homology_member using(homology_id) "
				  "right join member using (member_id) "
				  "where stable_id='" gene-stable-id "'"
				  " and source_name='ENSEMBLGENE'")))
    	query)
)

(defun homology-id->gene-stable-ids (homology-id &key species)
  "All gene IDs that are available from a particular homology ID shall be retrieved. Constrains can be given with a particular species that should be inspected only. The selection of homologues or paralogues should be constrained by the proper specification of species."
  (let ((query (concatenate 'string "select stable_id from homology "
			  	  "left join homology_member "
				  "where homology_id = "
				  (write-to-string homology-id)
				  (if species (concatenate 'string " and genome_db_id = "
							   		(if (integerp species) (write-to-string species)
									  	"damnit, where do I get this one from, now?")) nil))))
    query)
)
||#
