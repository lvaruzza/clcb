;; This packages aims at characterising the link from genes to SNPs

#||

mysql> desc variation;
+-------------------+-----------------------------------------------------------------+------+-----+---------+----------------+
| Field             | Type                                                            | Null | Key | Default | Extra          |
+-------------------+-----------------------------------------------------------------+------+-----+---------+----------------+
| variation_id      | int(10) unsigned                                                |      | PRI | NULL    | auto_increment |
| source_id         | int(10) unsigned                                                |      |     | 0       |                |
| name              | varchar(255)                                                    | YES  | MUL | NULL    |                |
| validation_status | set('cluster','freq','submitter','doublehit','hapmap','failed') | YES  |     | NULL    |                |
| ancestral_allele  | text                                                            | YES  |     | NULL    |                |
+-------------------+-----------------------------------------------------------------+------+-----+---------+----------------+
5 rows in set (0.04 sec)

mysql> desc allele;
+--------------+------------------+------+-----+---------+----------------+
| Field        | Type             | Null | Key | Default | Extra          |
+--------------+------------------+------+-----+---------+----------------+
| allele_id    | int(10) unsigned |      | PRI | NULL    | auto_increment |
| variation_id | int(10) unsigned |      | MUL | 0       |                |
| allele       | varchar(255)     | YES  |     | NULL    |                |
| frequency    | float            | YES  |     | NULL    |                |
| sample_id    | int(10) unsigned | YES  |     | NULL    |                |
+--------------+------------------+------+-----+---------+----------------+
5 rows in set (0.04 sec)

mysql> select * from variation limit 10;
+--------------+-----------+------+------------------------+------------------+
| variation_id | source_id | name | validation_status      | ancestral_allele |
+--------------+-----------+------+------------------------+------------------+
|            1 |         1 | rs3  | cluster,freq,submitter | C                |
|            2 |         1 | rs4  | cluster,freq,submitter | A                |
|            3 |         1 | rs5  | NULL                   | NULL             |
|            4 |         1 | rs6  | freq                   | C                |
|            5 |         1 | rs7  | NULL                   | A                |
|            6 |         1 | rs8  | cluster,freq,submitter | C                |
|            7 |         1 | rs9  | NULL                   | NULL             |
|            8 |         1 | rs10 | cluster,freq,submitter | C                |
|            9 |         1 | rs11 | NULL                   | C                |
|           10 |         1 | rs12 | submitter              | A                |
+--------------+-----------+------+------------------------+------------------+
10 rows in set (0.05 sec)

mysql> select * from allele limit 10;
+-----------+--------------+--------+-----------+-----------+
| allele_id | variation_id | allele | frequency | sample_id |
+-----------+--------------+--------+-----------+-----------+
|         1 |            1 | C      |  0.673913 |       630 |
|         2 |            1 | C      |      0.89 |        16 |
|         3 |            1 | C      |      0.89 |       662 |
|         4 |            1 | C      |      0.93 |        18 |
|         5 |            1 | C      |      0.95 |        17 |
|         6 |            1 | C      |     0.956 |       660 |
|         7 |            1 | C      |  0.958333 |       631 |
|         8 |            1 | C      |      0.96 |        15 |
|         9 |            1 | C      |     0.967 |       659 |
|        10 |            1 | C      |     0.978 |       661 |
+-----------+--------------+--------+-----------+-----------+
10 rows in set (0.04 sec)

select * from allele_group_allele join variation using (variation_id)
		join variation_group_variation using(variation_id)
		join variation_group using (variation_group_id)
		join variation_group_feature using (variation_group_id)
		join source on variation_group.source_id = source.source_id
limit 10;

select * from allele_group_allele join variation using (variation_id)
		join variation_feature using (variation_id) 
limit 10;

select * from allele join sample using (sample_id)
limit 10;


mysql> select * from variation_group_feature limit 10;
+----------------------------+---------------+------------------+----------------+-------------------+--------------------+----------------------+
variation_group_feature_id | seq_region_id | seq_region_start | seq_region_end | seq_region_strand | variation_group_id | variation_group_name |
+----------------------------+---------------+------------------+----------------+-------------------+--------------------+----------------------+
|                          1 |        226028 |         30190299 |       30233361 |                 1 |                  2 | PERLEGEN:B000001     |
|                          2 |        226028 |         30738720 |       30787661 |                -1 |                  3 | PERLEGEN:B000002     |
|                          3 |        226028 |         30978759 |       31009767 |                 1 |                  4 | PERLEGEN:B000003     |
|                          4 |        226028 |         36824334 |       36857707 |                 1 |                  5 | PERLEGEN:B000004     |
|                          5 |        226028 |         24318475 |       24340467 |                 1 |                  6 | PERLEGEN:B000005     |
|                          6 |        226028 |         23895381 |       23929680 |                 1 |                  7 | PERLEGEN:B000006     |
|                          7 |        226028 |         30532670 |       30567734 |                 1 |                  8 | PERLEGEN:B000007     |
|                          8 |        226028 |         24371534 |       24412984 |                 1 |                  9 | PERLEGEN:B000008     |
|                          9 |        226028 |         24192378 |       24206203 |                -1 |                 10 | PERLEGEN:B000009     |
|                         10 |        226028 |         27986621 |       28011953 |                 1 |                 11 | PERLEGEN:B000010     |
+----------------------------+---------------+------------------+----------------+-------------------+--------------------+----------------------+

mysql> select * from variation_group limit 10;
+--------------------+------------------+-----------+-----------+
variation_group_id | name             | source_id | type      |
+--------------------+------------------+-----------+-----------+
|                  1 | DBMHC:ABDR       |         1 | haplotype |
|                  2 | PERLEGEN:B000001 |         1 | haplotype |
|                  3 | PERLEGEN:B000002 |         1 | haplotype |
|                  4 | PERLEGEN:B000003 |         1 | haplotype |
|                  5 | PERLEGEN:B000004 |         1 | haplotype |
|                  6 | PERLEGEN:B000005 |         1 | haplotype |
|                  7 | PERLEGEN:B000006 |         1 | haplotype |
|                  8 | PERLEGEN:B000007 |         1 | haplotype |
|                  9 | PERLEGEN:B000008 |         1 | haplotype |
|                 10 | PERLEGEN:B000009 |         1 | haplotype |
+--------------------+------------------+-----------+-----------+
10 rows in set (0.04 sec)

mysql> desc variation_group;
+--------------------+-------------------------+------+-----+---------+----------------+
| Field              | Type                    | Null | Key | Default | Extra          |
+--------------------+-------------------------+------+-----+---------+----------------+
| variation_group_id | int(10) unsigned        |      | PRI | NULL    | auto_increment |
| name               | varchar(255)            | YES  | MUL | NULL    |                |
| source_id          | int(10) unsigned        |      |     | 0       |                |
| type               | enum('haplotype','tag') | YES  |     | NULL    |                |
+--------------------+-------------------------+------+-----+---------+----------------+
4 rows in set (0.04 sec)

mysql> select * from variation_feature limit 10;
+----------------------+---------------+------------------+----------------+-------------------+--------------+---------------+----------------+------------+-------+-----------+-------------------+-----------------------+
| variation_feature_id | seq_region_id | seq_region_start | seq_region_end | seq_region_strand | variation_id | allele_string | variation_name | map_weight | flags | source_id | validation_status | consequence_type      |
+----------------------+---------------+------------------+----------------+-------------------+--------------+---------------+----------------+------------+-------+-----------+-------------------+-----------------------+
|             12219625 |        225652 |             1454 |           1454 |                 1 |      2683771 | G/A           | rs3936235      |          1 | NULL  |         1 | cluster,doublehit | DOWNSTREAM            |
|             12219638 |        225652 |             1694 |           1694 |                 1 |      4714264 | G/A           | rs7466894      |          2 | NULL  |         1 | NULL              | DOWNSTREAM            |
|             12219640 |        225652 |             2641 |           2641 |                 1 |      5079970 | A/C           | rs7859283      |          2 | NULL  |         1 | NULL              | DOWNSTREAM            |
|             12219660 |        225652 |             3699 |           3698 |                -1 |     10239529 | -/G           | rs34505100     |          2 | NULL  |         1 | NULL              | DOWNSTREAM            |
|             12219628 |        225652 |             4240 |           4240 |                 1 |      3970869 | A/G           | rs6606566      |          2 | NULL  |         1 | cluster           | NON_SYNONYMOUS_CODING |
|             12219629 |        225652 |             4752 |           4752 |                 1 |      3970870 | C/T           | rs6606567      |          2 | NULL  |         1 | cluster           | INTRONIC              |
|             12219636 |        225652 |             5064 |           5064 |                 1 |      4676869 | G/A           | rs7389591      |          2 | NULL  |         1 | NULL              | UPSTREAM              |
|             12219641 |        225652 |             5586 |           5586 |                 1 |      5484481 | A/G           | rs9414697      |          2 | NULL  |         1 | NULL              | UPSTREAM              |
|             12219664 |        225652 |             5910 |           5909 |                -1 |     11052797 | -/G           | rs35408104     |          2 | NULL  |         1 | NULL              | UPSTREAM              |
|             12219634 |        225652 |             6306 |           6306 |                 1 |      4384489 | C/T           | rs7047363      |          2 | NULL  |         1 | NULL              | UPSTREAM              |
+----------------------+---------------+------------------+----------------+-------------------+--------------+---------------+----------------+------------+-------+-----------+-------------------+-----------------------+

||#

;; To access a strech of DNA 

Du weisst besser als ich, wie man von den seq_regions auf die absoluten Koordinaten kommt und wieder zurueck...

(defun chromosome+interval->snps (chromsome interval) 
  "Returns SNPs located in that interval."
  (let (query "select seq_region_id, seq_region_start, seq_region_end, seq_region_strand, variation_id, variation_name, consequence_type from variation_feature join homo_sapiens_core_47_36i.seq_region using (seq_region_id) where ....)
    ))
