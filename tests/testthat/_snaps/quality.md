# quality module works as expected in the test app

    Code
      res
    Output
      $message
      [1] "Please change gene filters to ensure that there are at least 2 genes"
      
      $call
      [1] "NULL"
      
      $type
      [1] "shiny.silent.error" "validation"        
      

---

    Code
      cat(res)
    Output
         y count       x    xmin    xmax      density ncount ndensity flipped_aes
      1  1     1 1781977 1772292 1791661 8.604677e-06      1        1       FALSE
      2  0     0 1801346 1791661 1811031 0.000000e+00      0        0       FALSE
      3  0     0 1820715 1811031 1830400 0.000000e+00      0        0       FALSE
      4  1     1 1840084 1830400 1849769 8.604677e-06      1        1       FALSE
      5  1     1 1859454 1849769 1869138 8.604677e-06      1        1       FALSE
      6  0     0 1878823 1869138 1888508 0.000000e+00      0        0       FALSE
      7  0     0 1898192 1888508 1907877 0.000000e+00      0        0       FALSE
      8  0     0 1917562 1907877 1927246 0.000000e+00      0        0       FALSE
      9  0     0 1936931 1927246 1946616 0.000000e+00      0        0       FALSE
      10 0     0 1956300 1946616 1965985 0.000000e+00      0        0       FALSE
      11 1     1 1975670 1965985 1985354 8.604677e-06      1        1       FALSE
      12 0     0 1995039 1985354 2004724 0.000000e+00      0        0       FALSE
      13 0     0 2014408 2004724 2024093 0.000000e+00      0        0       FALSE
      14 0     0 2033778 2024093 2043462 0.000000e+00      0        0       FALSE
      15 0     0 2053147 2043462 2062832 0.000000e+00      0        0       FALSE
      16 0     0 2072516 2062832 2082201 0.000000e+00      0        0       FALSE
      17 0     0 2091886 2082201 2101570 0.000000e+00      0        0       FALSE
      18 0     0 2111255 2101570 2120939 0.000000e+00      0        0       FALSE
      19 1     1 2130624 2120939 2140309 8.604677e-06      1        1       FALSE
      20 0     0 2149993 2140309 2159678 0.000000e+00      0        0       FALSE
      21 0     0 2169363 2159678 2179047 0.000000e+00      0        0       FALSE
      22 0     0 2188732 2179047 2198417 0.000000e+00      0        0       FALSE
      23 0     0 2208101 2198417 2217786 0.000000e+00      0        0       FALSE
      24 0     0 2227471 2217786 2237155 0.000000e+00      0        0       FALSE
      25 0     0 2246840 2237155 2256525 0.000000e+00      0        0       FALSE
      26 0     0 2266209 2256525 2275894 0.000000e+00      0        0       FALSE
      27 0     0 2285579 2275894 2295263 0.000000e+00      0        0       FALSE
      28 0     0 2304948 2295263 2314633 0.000000e+00      0        0       FALSE
      29 0     0 2324317 2314633 2334002 0.000000e+00      0        0       FALSE
      30 1     1 2343687 2334002 2353371 8.604677e-06      1        1       FALSE
         PANEL group ymin ymax colour     fill linewidth linetype alpha
      1      1    -1    0    1     NA darkgrey       0.5        1    NA
      2      1    -1    0    0     NA darkgrey       0.5        1    NA
      3      1    -1    0    0     NA darkgrey       0.5        1    NA
      4      1    -1    0    1     NA darkgrey       0.5        1    NA
      5      1    -1    0    1     NA darkgrey       0.5        1    NA
      6      1    -1    0    0     NA darkgrey       0.5        1    NA
      7      1    -1    0    0     NA darkgrey       0.5        1    NA
      8      1    -1    0    0     NA darkgrey       0.5        1    NA
      9      1    -1    0    0     NA darkgrey       0.5        1    NA
      10     1    -1    0    0     NA darkgrey       0.5        1    NA
      11     1    -1    0    1     NA darkgrey       0.5        1    NA
      12     1    -1    0    0     NA darkgrey       0.5        1    NA
      13     1    -1    0    0     NA darkgrey       0.5        1    NA
      14     1    -1    0    0     NA darkgrey       0.5        1    NA
      15     1    -1    0    0     NA darkgrey       0.5        1    NA
      16     1    -1    0    0     NA darkgrey       0.5        1    NA
      17     1    -1    0    0     NA darkgrey       0.5        1    NA
      18     1    -1    0    0     NA darkgrey       0.5        1    NA
      19     1    -1    0    1     NA darkgrey       0.5        1    NA
      20     1    -1    0    0     NA darkgrey       0.5        1    NA
      21     1    -1    0    0     NA darkgrey       0.5        1    NA
      22     1    -1    0    0     NA darkgrey       0.5        1    NA
      23     1    -1    0    0     NA darkgrey       0.5        1    NA
      24     1    -1    0    0     NA darkgrey       0.5        1    NA
      25     1    -1    0    0     NA darkgrey       0.5        1    NA
      26     1    -1    0    0     NA darkgrey       0.5        1    NA
      27     1    -1    0    0     NA darkgrey       0.5        1    NA
      28     1    -1    0    0     NA darkgrey       0.5        1    NA
      29     1    -1    0    0     NA darkgrey       0.5        1    NA
      30     1    -1    0    1     NA darkgrey       0.5        1    NA

---

    Code
      cat(res)
    Output
          x        y PANEL group flipped_aes ymin     ymax xmin  xmax colour   fill
      1   1 17.72389     1     1       FALSE    0 17.72389 0.55  1.45     NA grey35
      2   2 14.37759     1     2       FALSE    0 14.37759 1.55  2.45     NA grey35
      3   3 14.20068     1     3       FALSE    0 14.20068 2.55  3.45     NA grey35
      4   4 14.17412     1     4       FALSE    0 14.17412 3.55  4.45     NA grey35
      5   5 14.01385     1     5       FALSE    0 14.01385 4.55  5.45     NA grey35
      6   6 13.44916     1     6       FALSE    0 13.44916 5.55  6.45     NA grey35
      7   7 13.27510     1     7       FALSE    0 13.27510 6.55  7.45     NA grey35
      8   8 13.17301     1     8       FALSE    0 13.17301 7.55  8.45     NA grey35
      9   9 13.03098     1     9       FALSE    0 13.03098 8.55  9.45     NA grey35
      10 10 12.93251     1    10       FALSE    0 12.93251 9.55 10.45     NA grey35
         linewidth linetype alpha
      1        0.5        1    NA
      2        0.5        1    NA
      3        0.5        1    NA
      4        0.5        1    NA
      5        0.5        1    NA
      6        0.5        1    NA
      7        0.5        1    NA
      8        0.5        1    NA
      9        0.5        1    NA
      10       0.5        1    NA

