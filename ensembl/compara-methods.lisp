
;; The aim of these classes is mostly to add links between ortholog genes.

+||

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


select * from homology left join homology_member using(homology_id) left join member using (member_id) left join method_link_species_set on (homology.method_link_species_set_id=method_link_species_set.method_link_species_set_id) left join species_set using (species_set_id) left join genome_db using (genome_db_id) where homology.description='ortholog_one2one' and genome_db.name='Homo sapiens' and member.source_name='ENSEMBLGENE' limit 30;

(defun gene-stable-id->homology-id (gene-stable-id &key (ortholog T) (paralog nil))
  "Retrieval of set of homolog IDs that a gene is involved in. The optional parameters ortholog and paralog may be set to NIL if there shall be constraints for the links established."
  (let query "select homology_id from homology right join homology_member using(homology_id) right join member using (member_id) where stable_id='" gene-stable-id "' source_name='ENSEMBLGENE'")
)

(defun homology-id->gene-stable-id (homology-id &key (species nil)) 
  "All gene IDs that are available from a particular homology ID shall be retrieved. Constrains can be given with a particular species that should be inspected only."
  (let query "select stable_id from homology left join homology_member where homology_id=" homology-id ")
)

||+
