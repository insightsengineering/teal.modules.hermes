# km module works as expected in the test app

    Code
      cat(res)
    Output
      $binned_adtte
                       USUBJID low_depth_flag tech_failure_flag Filename
      3   AB12345-CHN-1-id-307          FALSE             FALSE     eset
      9  AB12345-CHN-15-id-201          FALSE             FALSE     eset
      11   AB12345-CHN-4-id-73          FALSE             FALSE     eset
      18   AB12345-CHN-7-id-28          FALSE             FALSE     eset
      24 AB12345-NGA-11-id-173           TRUE             FALSE     eset
      30 AB12345-PAK-11-id-268          FALSE             FALSE     eset
      34   AB12345-RUS-1-id-52          FALSE             FALSE     eset
      39  AB12345-USA-1-id-261          FALSE             FALSE     eset
      41   AB12345-USA-1-id-45          FALSE             FALSE     eset
                 AGEGRP AGE18                STDDRS
      3  12 - <18 years  < 18                 DEATH
      9     >= 18 years >= 18                 DEATH
      11  6 - <12 years  < 18                 DEATH
      18 12 - <18 years  < 18                      
      24  6 - <12 years  < 18                 DEATH
      30    >= 18 years >= 18 WITHDRAWAL BY SUBJECT
      34  6 - <12 years  < 18                 DEATH
      39 12 - <18 years  < 18                      
      41    >= 18 years >= 18                 DEATH
                                     STDDRSD    STDSSDT                TRTDRS
      3  DEATH DUE TO PROGRESSION OF DISEASE 07/24/2016   PROGRESSIVE DISEASE
      9  DEATH DUE TO PROGRESSION OF DISEASE 08/12/2016   PROGRESSIVE DISEASE
      11 DEATH DUE TO PROGRESSION OF DISEASE 04/19/2016   PROGRESSIVE DISEASE
      18                                                  PROGRESSIVE DISEASE
      24 DEATH DUE TO PROGRESSION OF DISEASE 09/11/2016   PROGRESSIVE DISEASE
      30               WITHDRAWAL BY SUBJECT 07/05/2016 WITHDRAWAL BY SUBJECT
      34 DEATH DUE TO PROGRESSION OF DISEASE 10/23/2016   PROGRESSIVE DISEASE
      39                                                  PROGRESSIVE DISEASE
      41 DEATH DUE TO PROGRESSION OF DISEASE 01/08/2016   PROGRESSIVE DISEASE
                        TRTDRSD BHDCIRC BHDCIRCU ADAFL BLANP BKPS BLKS BTANNER
      3  PROGRESSION OF DISEASE      NA              Y    NA   80   80      NA
      9  PROGRESSION OF DISEASE      NA              Y    NA   90   90      NA
      11 PROGRESSION OF DISEASE      NA              Y    70   NA   70      NA
      18 PROGRESSION OF DISEASE      NA              Y    NA   90   90      NA
      24 PROGRESSION OF DISEASE      NA              Y    NA  100  100      NA
      30  WITHDRAWAL BY SUBJECT      NA              Y    70   NA   70      NA
      34 PROGRESSION OF DISEASE      NA              Y   100   NA  100      NA
      39 PROGRESSION OF DISEASE      NA              Y    NA   90   90      NA
      41 PROGRESSION OF DISEASE      NA              Y    NA   90   90      NA
                  FRPST   DURIDX    DURSAF     DURSUR LNTHRPY AENCIFL STUDYID
      3  POST-MENARCHAL 61.66735 1.6755647  5.1581109       5      NA AB12345
      9                 10.84189 0.9856263  3.0554415       4      NA AB12345
      11                45.66735 1.6755647  3.1540041      10      NA AB12345
      18 POST-MENARCHAL 19.54825 1.7412731  9.6919918       4      NA AB12345
      24                99.28542 3.5811088  6.3408624       7      NA AB12345
      30                16.09856 0.9856263  0.6899384       2      NA AB12345
      34  PRE-MENARCHAL       NA 1.6755647 10.9404517       3      NA AB12345
      39                41.62628 1.6755647 18.8583162       3      NA AB12345
      41                45.73306 0.6570842  2.2997947       2      NA AB12345
                  RFSTDTC          RFENDTC         RFXSTDTC         RFXENDTC
      3  2016-03-10T14:05 2016-03-31T15:49 2016-03-10T14:05 2016-03-31T15:49
      9  2016-05-31T14:10 2016-05-31T14:10 2016-05-31T14:10 2016-05-31T14:10
      11 2016-01-25T14:15 2016-02-15T10:40 2016-01-25T14:15 2016-02-15T10:40
      18 2016-12-06T13:09 2016-12-29T15:46 2016-12-06T13:09 2016-12-29T15:46
      24 2016-03-21T15:40 2016-06-08T18:00 2016-03-21T15:40 2016-06-08T18:00
      30 2016-06-15T12:45 2016-06-15T12:45 2016-06-15T12:45 2016-06-15T12:45
      34 2015-12-01T12:25 2015-12-22T14:10 2015-12-01T12:25 2015-12-22T14:10
      39 2016-02-08T12:37 2016-02-29T14:15 2016-02-08T12:37 2016-02-29T14:15
      41 2015-11-05T11:00 2015-11-05T11:00 2015-11-05T11:00 2015-11-05T11:00
            RFICDTC   RFPENDTC     DTHDTC DTHFL SITEID  INVID AGE  AGEU SEX
      3  2016-02-18 2016-07-24 2016-07-24     Y 283495 223804  12 YEARS   F
      9  2016-05-11 2016-08-12 2016-08-12     Y 282087 468105  27 YEARS   F
      11 2016-01-14 2016-04-19 2016-04-19     Y 280959  20842  10 YEARS   F
      18 2016-11-28                             282703 301818  17 YEARS   M
      24 2016-03-02 2016-09-11 2016-09-11     Y 283497 241874   7 YEARS   F
      30 2016-06-14 2016-07-05                  283662 244110  19 YEARS   F
      34 2015-11-25 2016-10-23 2016-10-23     Y 283971 235545  11 YEARS   F
      39 2016-02-01                             281049 457179  16 YEARS   F
      41 2015-10-30 2016-01-08 2016-01-08     Y 283971 235545  19 YEARS   F
                              RACE                 ETHNIC ARMCD       ARM ACTARMCD
      3                      WHITE NOT HISPANIC OR LATINO  COH3  COHORT 3     COH3
      9                    UNKNOWN           NOT REPORTED COH9E COHORT 9E    COH9E
      11                   UNKNOWN           NOT REPORTED  COH3  COHORT 3     COH3
      18                   UNKNOWN           NOT REPORTED  COH2  COHORT 2     COH2
      24 BLACK OR AFRICAN AMERICAN     HISPANIC OR LATINO  COH6  COHORT 6     COH6
      30                     ASIAN     HISPANIC OR LATINO  COH6  COHORT 6     COH6
      34                     WHITE NOT HISPANIC OR LATINO COH7A COHORT 7A    COH7A
      39                     ASIAN NOT HISPANIC OR LATINO  COH1  COHORT 1     COH1
      41                   UNKNOWN     HISPANIC OR LATINO  COH6  COHORT 6     COH6
           ACTARM COUNTRY      DMDTC DMDY BAGE BAGEU   BWT BWTU BHT BHTU     BBMI
      3  COHORT 3     CHN 2016-02-18  -21   12 YEARS  50.0   kg 157   cm 20.28480
      9  COHORT 9     CHN 2016-05-25   -6   27 YEARS  61.6   kg 173   cm 20.58204
      11 COHORT 3     CHN 2016-01-14  -11   10 YEARS  29.4   kg 148   cm 13.42221
      18 COHORT 2     CHN 2016-11-28   -8   17 YEARS  38.2   kg 151   cm 16.75365
      24 COHORT 6     NGA 2016-03-02  -19    7 YEARS  53.9   kg 176   cm 17.40057
      30 COHORT 6     PAK 2016-06-14   -1   19 YEARS  51.0   kg 152   cm 22.07410
      34 COHORT 7     RUS 2015-11-25   -6   11 YEARS  36.2   kg 130   cm 21.42012
      39 COHORT 1     USA 2016-02-01   -7   16 YEARS 104.7   kg 172   cm 35.39075
      41 COHORT 6     USA 2015-10-30   -6   19 YEARS  57.0   kg 172   cm 19.26717
         ITTFL SAFFL    INFCODT     RANDDT          TRTSDTC             TRTSDTM
      3      Y     Y 2016-02-18 2016-03-09 2016-03-10T14:05 2016-03-10 14:05:00
      9      Y     Y 2016-05-11 2016-05-31 2016-05-31T14:10 2016-05-31 14:10:00
      11     Y     Y 2016-01-14 2016-01-22 2016-01-25T14:15 2016-01-25 14:15:00
      18     Y     Y 2016-11-28 2016-12-02 2016-12-06T13:09 2016-12-06 13:09:00
      24     Y     Y 2016-03-02 2016-03-21 2016-03-21T15:40 2016-03-21 15:40:00
      30     Y     Y 2016-06-14 2016-06-15 2016-06-15T12:45 2016-06-15 12:45:00
      34     Y     Y 2015-11-25 2015-12-01 2015-12-01T12:25 2015-12-01 12:25:00
      39     Y     Y 2016-02-01 2016-02-03 2016-02-08T12:37 2016-02-08 12:37:00
      41     Y     Y 2015-10-30 2015-11-05 2015-11-05T11:00 2015-11-05 11:00:00
         TRTSTMF             TRTEDTM TRTETMF TRTDUR DISCSTUD DISCDEAT DISCAE DISTRTFL
      3        S 2016-03-31 16:55:59       S     22        Y        Y      N        Y
      9        S 2016-05-31 15:10:59       S      1        Y        Y      N        Y
      11       S 2016-02-15 11:10:59       S     22        Y        Y      N        Y
      18       S 2016-12-29 16:19:59       S     24        N        N      N        Y
      24       S 2016-06-08 18:30:59       S     80        Y        Y      N        Y
      30       S 2016-06-15 13:45:59       S      1        Y        N      N        Y
      34       S 2015-12-22 14:40:59       S     22        Y        Y      N        Y
      39       S 2016-02-29 14:48:59       S     22        N        N      N        Y
      41       S 2015-11-05 12:00:59       S      1        Y        Y      N        Y
         AEWITHFL     ALIVDT
      3         N 2016-07-24
      9         N 2016-08-12
      11        N 2016-04-19
      18        N 2017-09-19
      24        N 2016-09-11
      30        N 2016-07-05
      34        N 2016-10-23
      39        N 2017-08-28
      41        N 2016-01-08
                                                                COHORT
      3                                       Cohort 3 (NEUROBLASTOMA)
      9  Cohort 9 (OTHER TUMOR TYPES WITH DOCUMENTED PD-L1 EXPRESSION)
      11                                      Cohort 3 (NEUROBLASTOMA)
      18                                   Cohort 2 (HODGKIN LYMPHOMA)
      24                                       Cohort 6 (OSTEOSARCOMA)
      30                                       Cohort 6 (OSTEOSARCOMA)
      34                                  Cohort 7 (RHABDOMYOSARCOMA )
      39                                      Cohort 1 (EWING SARCOMA)
      41                                       Cohort 6 (OSTEOSARCOMA)
                                                                                              TTYPE
      3                                                                               NEUROBLASTOMA
      9  GERM CELL TUMOR - YOLK SAC TUMOR (ENDODERMAL SINUS TUMOR) WITH DOCUMENTED PD-L1 EXPRESSION
      11                                                                              NEUROBLASTOMA
      18                                                                           HODGKIN LYMPHOMA
      24                                                                               OSTEOSARCOMA
      30                                                                               OSTEOSARCOMA
      34                                                                RHABDOMYOSARCOMA - ALVEOLAR
      39                                                                              EWING SARCOMA
      41                                                                               OSTEOSARCOMA
         STDSSDY                SUBJID Mean.ABCF2..ABO.         INVNAM         TRT01P
      3      137  AB12345-CHN-1-id-307         7.673103  Dr. CHN-1 Doe     B: Placebo
      9       74 AB12345-CHN-15-id-201         7.433943 Dr. CHN-15 Doe C: Combination
      11      86   AB12345-CHN-4-id-73         7.856089  Dr. CHN-4 Doe      A: Drug X
      18      NA   AB12345-CHN-7-id-28         7.454552  Dr. CHN-7 Doe     B: Placebo
      24     175 AB12345-NGA-11-id-173         7.702519 Dr. NGA-11 Doe C: Combination
      30      21 AB12345-PAK-11-id-268         7.324474 Dr. PAK-11 Doe      A: Drug X
      34     328   AB12345-RUS-1-id-52         7.410471  Dr. RUS-1 Doe      A: Drug X
      39      NA  AB12345-USA-1-id-261         7.662349  Dr. USA-1 Doe     B: Placebo
      41      65   AB12345-USA-1-id-45         7.780904  Dr. USA-1 Doe      A: Drug X
                 TRT01A     TRT02P         TRT02A       REGION1 STRATA1 STRATA2
      3      B: Placebo B: Placebo C: Combination          Asia       C      S1
      9  C: Combination B: Placebo C: Combination          Asia       C      S2
      11      A: Drug X B: Placebo C: Combination          Asia       A      S1
      18     B: Placebo B: Placebo     B: Placebo          Asia       C      S2
      24 C: Combination B: Placebo      A: Drug X        Africa       C      S2
      30      A: Drug X B: Placebo      A: Drug X          Asia       C      S2
      34      A: Drug X B: Placebo C: Combination       Eurasia       C      S2
      39     B: Placebo B: Placebo     B: Placebo North America       C      S1
      41      A: Drug X  A: Drug X      A: Drug X North America       C      S1
             BMRKR1 BMRKR2 BMEASIFL BEP01FL           TRT01SDTM           TRT01EDTM
      3   4.5749910    LOW        Y       N 2019-03-11 08:01:26 2021-02-18 02:48:40
      9   6.9067988 MEDIUM        Y       N 2019-03-05 15:24:07 2021-02-18 22:06:48
      11  2.8631240 MEDIUM        Y       Y 2019-03-17 14:18:17 2021-02-14 13:35:43
      18 11.1444470 MEDIUM        N       N 2019-03-11 09:11:52 2021-03-10 21:11:52
      24  4.9972257    LOW        Y       Y 2019-03-10 09:35:30 2021-03-09 21:35:30
      30  2.8201408 MEDIUM        N       N 2019-03-21 23:53:53 2021-03-21 11:53:53
      34  7.2063482   HIGH        N       N 2019-03-18 09:38:55 2021-02-13 06:30:28
      39  2.8551642   HIGH        Y       N 2019-03-06 17:21:21 2021-03-06 05:21:21
      41  0.4635604    LOW        N       N 2019-03-06 06:32:29 2021-03-05 18:32:29
                   TRT02SDTM           TRT02EDTM            AP01SDTM
      3  2021-02-18 02:48:40 2022-02-18 08:48:40 2019-03-11 08:01:26
      9  2021-02-18 22:06:48 2022-02-19 04:06:48 2019-03-05 15:24:07
      11 2021-02-14 13:35:43 2022-02-14 19:35:43 2019-03-17 14:18:17
      18 2021-03-10 21:11:52 2022-03-11 03:11:52 2019-03-11 09:11:52
      24 2021-03-09 21:35:30 2022-03-10 03:35:30 2019-03-10 09:35:30
      30 2021-03-21 11:53:53 2022-03-21 17:53:53 2019-03-21 23:53:53
      34 2021-02-13 06:30:28 2022-02-13 12:30:28 2019-03-18 09:38:55
      39 2021-03-06 05:21:21 2022-03-06 11:21:21 2019-03-06 17:21:21
      41 2021-03-05 18:32:29 2022-03-06 00:32:29 2019-03-06 06:32:29
                    AP01EDTM            AP02SDTM            AP02EDTM       EOSSTT
      3  2021-02-18 02:48:40 2021-02-18 02:48:40 2022-02-18 08:48:40 DISCONTINUED
      9  2021-02-18 22:06:48 2021-02-18 22:06:48 2022-02-19 04:06:48 DISCONTINUED
      11 2021-02-14 13:35:43 2021-02-14 13:35:43 2022-02-14 19:35:43 DISCONTINUED
      18 2021-03-10 21:11:52 2021-03-10 21:11:52 2022-03-11 03:11:52    COMPLETED
      24 2021-03-09 21:35:30 2021-03-09 21:35:30 2022-03-10 03:35:30    COMPLETED
      30 2021-03-21 11:53:53 2021-03-21 11:53:53 2022-03-21 17:53:53    COMPLETED
      34 2021-02-13 06:30:28 2021-02-13 06:30:28 2022-02-13 12:30:28 DISCONTINUED
      39 2021-03-06 05:21:21 2021-03-06 05:21:21 2022-03-06 11:21:21    COMPLETED
      41 2021-03-05 18:32:29 2021-03-05 18:32:29 2022-03-06 00:32:29    COMPLETED
               EOTSTT      EOSDT EOSDY          DCSREAS      DTHDT
      3  DISCONTINUED 2022-02-18  1076            DEATH 2022-04-06
      9  DISCONTINUED 2022-02-19  1082            DEATH 2022-02-22
      11 DISCONTINUED 2022-02-14  1066 LACK OF EFFICACY       <NA>
      18    COMPLETED 2022-03-11  1096             <NA>       <NA>
      24    COMPLETED 2022-03-10  1096             <NA>       <NA>
      30    COMPLETED 2022-03-21  1096             <NA>       <NA>
      34 DISCONTINUED 2022-02-13  1064            DEATH 2022-02-20
      39    COMPLETED 2022-03-06  1096             <NA>       <NA>
      41    COMPLETED 2022-03-06  1096             <NA>       <NA>
                     DTHCAUS              DTHCAT LDDTHELD LDDTHGR1   LSTALVDT DTHADY
      3    LOST TO FOLLOW UP               OTHER       47      >30 2022-04-06   1121
      9        ADVERSE EVENT       ADVERSE EVENT        3     <=30 2022-02-22   1084
      11                <NA>                <NA>       NA     <NA> 2022-03-07     NA
      18                <NA>                <NA>       NA     <NA> 2022-03-31     NA
      24                <NA>                <NA>       NA     <NA> 2022-03-30     NA
      30                <NA>                <NA>       NA     <NA> 2022-04-02     NA
      34 DISEASE PROGRESSION PROGRESSIVE DISEASE        7     <=30 2022-02-20   1069
      39                <NA>                <NA>       NA     <NA> 2022-03-30     NA
      41                <NA>                <NA>       NA     <NA> 2022-03-30     NA
         ADTHAUT ASEQ TTESEQ                     PARAM PARAMCD      AVAL AVALU
      3     <NA>    4      4 Progression Free Survival     PFS 239.08590  DAYS
      9      Yes    2      2 Progression Free Survival     PFS 248.53707  DAYS
      11    <NA>    3      3 Progression Free Survival     PFS 420.38459  DAYS
      18    <NA>    3      3 Progression Free Survival     PFS  30.04978  DAYS
      24    <NA>    2      2 Progression Free Survival     PFS 382.86110  DAYS
      30    <NA>    1      1 Progression Free Survival     PFS 326.40156  DAYS
      34     Yes    1      1 Progression Free Survival     PFS  45.07870  DAYS
      39    <NA>    2      2 Progression Free Survival     PFS 899.41424  DAYS
      41    <NA>    3      3 Progression Free Survival     PFS 131.76520  DAYS
                        ADTM  ADY CNSR                    EVNTDESC
      3  2022-02-16 08:01:26 1073    1       Last Tumor Assessment
      9  2020-04-05 15:24:07  397    0         Disease Progression
      11 2020-09-20 14:18:17  553    0                       Death
      18 2020-10-02 09:11:52  571    1 Last Date Known To Be Alive
      24 2021-09-04 09:35:30  909    0         Disease Progression
      30 2019-12-14 23:53:53  268    1       Last Tumor Assessment
      34 2019-11-14 09:38:55  241    0         Disease Progression
      39 2020-07-15 17:21:21  497    0                       Death
      41 2020-06-28 06:32:29  480    0                       Death
                           CNSDTDSC lgTMATRSK is_event gene_factor
      3            Clinical Cut Off        NA    FALSE  (50%,100%]
      9                                    NA     TRUE    [0%,50%]
      11                                   NA     TRUE  (50%,100%]
      18           Clinical Cut Off        NA    FALSE    [0%,50%]
      24                                   NA     TRUE  (50%,100%]
      30 End of AE Reporting Period        NA    FALSE    [0%,50%]
      34                                   NA     TRUE    [0%,50%]
      39                                   NA     TRUE    [0%,50%]
      41                                   NA     TRUE  (50%,100%]
      
      $variables
      $variables$tte
      [1] "AVAL"
      
      $variables$is_event
      [1] "is_event"
      
      $variables$arm
      [1] "gene_factor"
      
      $variables$strat
      NULL

