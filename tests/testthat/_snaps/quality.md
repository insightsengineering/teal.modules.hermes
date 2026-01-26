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
         count       x    xmin    xmax      density ncount ndensity flipped_aes PANEL
      1      1 1777260 1767575 1786945 8.604677e-06      1        1       FALSE     1
      2      0 1796629 1786945 1806314 0.000000e+00      0        0       FALSE     1
      3      0 1815999 1806314 1825683 0.000000e+00      0        0       FALSE     1
      4      1 1835368 1825683 1845053 8.604677e-06      1        1       FALSE     1
      5      1 1854737 1845053 1864422 8.604677e-06      1        1       FALSE     1
      6      0 1874107 1864422 1883791 0.000000e+00      0        0       FALSE     1
      7      0 1893476 1883791 1903161 0.000000e+00      0        0       FALSE     1
      8      0 1912845 1903161 1922530 0.000000e+00      0        0       FALSE     1
      9      0 1932214 1922530 1941899 0.000000e+00      0        0       FALSE     1
      10     0 1951584 1941899 1961268 0.000000e+00      0        0       FALSE     1
      11     1 1970953 1961268 1980638 8.604677e-06      1        1       FALSE     1
      12     0 1990322 1980638 2000007 0.000000e+00      0        0       FALSE     1
      13     0 2009692 2000007 2019376 0.000000e+00      0        0       FALSE     1
      14     0 2029061 2019376 2038746 0.000000e+00      0        0       FALSE     1
      15     0 2048430 2038746 2058115 0.000000e+00      0        0       FALSE     1
      16     0 2067800 2058115 2077484 0.000000e+00      0        0       FALSE     1
      17     0 2087169 2077484 2096854 0.000000e+00      0        0       FALSE     1
      18     0 2106538 2096854 2116223 0.000000e+00      0        0       FALSE     1
      19     1 2125908 2116223 2135592 8.604677e-06      1        1       FALSE     1
      20     0 2145277 2135592 2154962 0.000000e+00      0        0       FALSE     1
      21     0 2164646 2154962 2174331 0.000000e+00      0        0       FALSE     1
      22     0 2184016 2174331 2193700 0.000000e+00      0        0       FALSE     1
      23     0 2203385 2193700 2213069 0.000000e+00      0        0       FALSE     1
      24     0 2222754 2213069 2232439 0.000000e+00      0        0       FALSE     1
      25     0 2242123 2232439 2251808 0.000000e+00      0        0       FALSE     1
      26     0 2261493 2251808 2271177 0.000000e+00      0        0       FALSE     1
      27     0 2280862 2271177 2290547 0.000000e+00      0        0       FALSE     1
      28     0 2300231 2290547 2309916 0.000000e+00      0        0       FALSE     1
      29     0 2319601 2309916 2329285 0.000000e+00      0        0       FALSE     1
      30     1 2338970 2329285 2348655 8.604677e-06      1        1       FALSE     1
         group y ymin ymax colour     fill linewidth linetype alpha width
      1     -1 1    0    1     NA darkgrey       0.5        1    NA   0.9
      2     -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      3     -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      4     -1 1    0    1     NA darkgrey       0.5        1    NA   0.9
      5     -1 1    0    1     NA darkgrey       0.5        1    NA   0.9
      6     -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      7     -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      8     -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      9     -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      10    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      11    -1 1    0    1     NA darkgrey       0.5        1    NA   0.9
      12    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      13    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      14    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      15    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      16    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      17    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      18    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      19    -1 1    0    1     NA darkgrey       0.5        1    NA   0.9
      20    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      21    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      22    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      23    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      24    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      25    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      26    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      27    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      28    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      29    -1 0    0    0     NA darkgrey       0.5        1    NA   0.9
      30    -1 1    0    1     NA darkgrey       0.5        1    NA   0.9

---

    Code
      cat(res)
    Output
          x        y PANEL group flipped_aes ymin     ymax xmin  xmax colour
      1   1 17.72389     1     1       FALSE    0 17.72389 0.55  1.45     NA
      2   2 14.37759     1     2       FALSE    0 14.37759 1.55  2.45     NA
      3   3 14.20068     1     3       FALSE    0 14.20068 2.55  3.45     NA
      4   4 14.17412     1     4       FALSE    0 14.17412 3.55  4.45     NA
      5   5 14.01385     1     5       FALSE    0 14.01385 4.55  5.45     NA
      6   6 13.44916     1     6       FALSE    0 13.44916 5.55  6.45     NA
      7   7 13.27510     1     7       FALSE    0 13.27510 6.55  7.45     NA
      8   8 13.17301     1     8       FALSE    0 13.17301 7.55  8.45     NA
      9   9 13.03098     1     9       FALSE    0 13.03098 8.55  9.45     NA
      10 10 12.93251     1    10       FALSE    0 12.93251 9.55 10.45     NA
              fill linewidth linetype alpha width
      1  #595959FF       0.5        1    NA   0.9
      2  #595959FF       0.5        1    NA   0.9
      3  #595959FF       0.5        1    NA   0.9
      4  #595959FF       0.5        1    NA   0.9
      5  #595959FF       0.5        1    NA   0.9
      6  #595959FF       0.5        1    NA   0.9
      7  #595959FF       0.5        1    NA   0.9
      8  #595959FF       0.5        1    NA   0.9
      9  #595959FF       0.5        1    NA   0.9
      10 #595959FF       0.5        1    NA   0.9

