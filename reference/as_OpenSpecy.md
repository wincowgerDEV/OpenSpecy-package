# Create `OpenSpecy` objects

Functions to check if an object is an OpenSpecy, or coerce it if
possible.

## Usage

``` r
as_OpenSpecy(x, ...)

# S3 method for class 'OpenSpecy'
as_OpenSpecy(x, session_id = FALSE, ...)

# S3 method for class 'list'
as_OpenSpecy(x, ...)

# S3 method for class 'hyperSpec'
as_OpenSpecy(x, ...)

# S3 method for class 'data.frame'
as_OpenSpecy(x, colnames = list(wavenumber = NULL, spectra = NULL), ...)

# Default S3 method
as_OpenSpecy(
  x,
  spectra,
  metadata = list(file_name = NULL, user_name = NULL, contact_info = NULL, organization =
    NULL, citation = NULL, spectrum_type = NULL, spectrum_identity = NULL, material_form
    = NULL, material_phase = NULL, material_producer = NULL, material_purity = NULL,
    material_quality = NULL, material_color = NULL, material_other = NULL, cas_number =
    NULL, instrument_used = NULL, instrument_accessories = NULL, instrument_mode = NULL,
    intensity_units = NULL, spectral_resolution = NULL, laser_light_used = NULL,
    number_of_accumulations = NULL, 
     total_acquisition_time_s = NULL,
    data_processing_procedure = NULL, level_of_confidence_in_identification = NULL,
    other_info = NULL, license = "CC BY-NC"),
  attributes = list(intensity_unit = NULL, derivative_order = NULL, baseline = NULL,
    spectra_type = NULL),
  coords = "gen_grid",
  session_id = FALSE,
  comma_decimal = FALSE,
  ...
)

is_OpenSpecy(x)

check_OpenSpecy(x)

OpenSpecy(x, ...)

gen_grid(n)
```

## Arguments

- x:

  depending on the method, a list with all OpenSpecy parameters, a
  vector with the wavenumbers for all spectra, or a data.frame with a
  full spectrum in the classic Open Specy format.

- session_id:

  logical. Whether to add a session ID to the metadata. The session ID
  is based on current session info so metadata of the same spectra will
  not return equal if session info changes. Sometimes that is desirable.

- colnames:

  names of the wavenumber column and spectra column, makes assumptions
  based on column names or placement if `NULL`.

- spectra:

  spectral intensities formatted as a data.table with one column per
  spectrum.

- metadata:

  metadata for each spectrum with one row per spectrum, see details.

- attributes:

  a list of attributes describing critical aspects for interpreting the
  spectra. see details.

- coords:

  spatial coordinates for the spectra.

- comma_decimal:

  logical(1) whether commas may represent decimals.

- n:

  number of spectra to generate the spatial coordinate grid with.

- ...:

  additional arguments passed to submethods.

## Value

`as_OpenSpecy()` and `OpenSpecy()` returns three part lists described in
details. `is_OpenSpecy()` returns `TRUE` if the object is an OpenSpecy
and `FALSE` if not. `gen_grid()` returns a `data.table` with `x` and `y`
coordinates to use for generating a spatial grid for the spectra if one
is not specified in the data.

## Details

`as_OpenSpecy()` converts spectral datasets to a three part list; the
first with a vector of the wavenumbers of the spectra, the second with a
`data.table` of all spectral intensities ordered as columns, the third
item is another `data.table` with any metadata the user provides or is
harvested from the files themselves.

The `metadata` argument may contain a named list with the following
details (`*` = minimum recommended).

- `file_name*`:

  The file name, defaults to
  [`basename()`](https://rdrr.io/r/base/basename.html) if not specified

- `user_name*`:

  User name, e.g. "Win Cowger"

- `contact_info`:

  Contact information, e.g. "1-513-673-8956, wincowger@gmail.com"

- `organization`:

  Affiliation, e.g. "University of California, Riverside"

- `citation`:

  Data citation, e.g. "Primpke, S., Wirth, M., Lorenz, C., & Gerdts, G.
  (2018). Reference database design for the automated analysis of
  microplastic samples based on Fourier transform infrared (FTIR)
  spectroscopy. *Analytical and Bioanalytical Chemistry*.
  [doi:10.1007/s00216-018-1156-x](https://doi.org/10.1007/s00216-018-1156-x)
  "

- `spectrum_type*`:

  Raman or FTIR

- `spectrum_identity*`:

  Material/polymer analyzed, e.g. "Polystyrene"

- `material_form`:

  Form of the material analyzed, e.g. textile fiber, rubber band,
  sphere, granule

- `material_phase`:

  Phase of the material analyzed (liquid, gas, solid)

- `material_producer`:

  Producer of the material analyzed, e.g. Dow

- `material_purity`:

  Purity of the material analyzed, e.g. 99.98%

- `material_quality`:

  Quality of the material analyzed, e.g. consumer product, manufacturer
  material, analytical standard, environmental sample

- `material_color`:

  Color of the material analyzed, e.g. blue, \#0000ff, (0, 0, 255)

- material_other:

  Other material description, e.g. 5 µm diameter fibers, 1 mm spherical
  particles

- `cas_number`:

  CAS number, e.g. 9003-53-6

- `instrument_used`:

  Instrument used, e.g. Horiba LabRam

- instrument_accessories:

  Instrument accessories, e.g. Focal Plane Array, CCD

- `instrument_mode`:

  Instrument modes/settings, e.g. transmission, reflectance

- `intensity_units*`:

  Units of the intensity values for the spectrum, options transmittance,
  reflectance, absorbance

- `spectral_resolution`:

  Spectral resolution, e.g. 4/cm

- `laser_light_used`:

  Wavelength of the laser/light used, e.g. 785 nm

- `number_of_accumulations`:

  Number of accumulations, e.g 5

- `total_acquisition_time_s`:

  Total acquisition time (s), e.g. 10 s

- `data_processing_procedure`:

  Data processing procedure, e.g. spikefilter, baseline correction, none

- `level_of_confidence_in_identification`:

  Level of confidence in identification, e.g. 99%

- `other_info`:

  Other information

- `license`:

  The license of the shared spectrum; defaults to `"CC BY-NC"` (see
  <https://creativecommons.org/licenses/by-nc/4.0/> for details). Any
  other creative commons license is allowed, for example, CC0 or CC BY

- `session_id`:

  A unique user and session identifier; populated automatically with
  `paste(digest(Sys.info()), digest(sessionInfo()), sep = "/")`

- `file_id`:

  A unique file identifier; populated automatically with
  `digest(object[c("wavenumber", "spectra")])`

The `attributes` argument may contain a named list with the following
details, when set, they will be used to automate transformations and
warning messages:

- `intensity_units`:

  supported options include `"absorbance"`, `"transmittance"`, or
  `"reflectance"`

- `derivative_order`:

  supported options include `"0"`, `"1"`, or `"2"`

- `baseline`:

  supported options include `"raw"` or `"nobaseline"`

- `spectra_type`:

  supported options include `"ftir"` or `"raman"`

## See also

[`read_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/io_spec.md)
for reading `OpenSpecy` objects.

## Author

Zacharias Steinmetz, Win Cowger

## Examples

``` r
data("raman_hdpe")

# Inspect the spectra
raman_hdpe # see how OpenSpecy objects print.
#>      wavenumber intensity
#>           <num>     <int>
#>   1:    301.040        26
#>   2:    304.632        50
#>   3:    308.221        48
#>   4:    311.810        45
#>   5:    315.398        46
#>  ---                     
#> 960:   3187.990        71
#> 961:   3190.520        71
#> 962:   3193.060        75
#> 963:   3195.590        75
#> 964:   3198.120        67
#> 
#> $metadata
#>        x     y  user_name spectrum_type spectrum_identity      organization
#>    <int> <int>     <char>        <char>            <char>            <char>
#> 1:     1     1 Win Cowger         Raman              HDPE Horiba Scientific
#>     license                                                        session_id
#>      <char>                                                            <char>
#> 1: CC BY-NC 5728ddde4f649fd71f6f487fc5ad8d80/dc85257201307a131e71d9ec24aaccbf
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b
raman_hdpe$wavenumber # look at just the wavenumbers of the spectra.
#>   [1]  301.040  304.632  308.221  311.810  315.398  318.983  322.566  326.150
#>   [9]  329.732  333.311  336.889  340.467  344.042  347.618  351.190  354.760
#>  [17]  358.332  361.900  365.466  369.031  372.597  376.160  379.721  383.280
#>  [25]  386.839  390.396  393.951  397.505  401.058  404.610  408.160  411.708
#>  [33]  415.256  418.801  422.346  425.887  429.429  432.970  436.507  440.044
#>  [41]  443.581  447.116  450.647  454.180  457.708  461.238  464.764  468.291
#>  [49]  471.815  475.339  478.859  482.379  485.899  489.416  492.931  496.447
#>  [57]  499.960  503.471  506.981  510.489  513.999  517.503  521.009  524.510
#>  [65]  528.014  531.513  535.014  538.510  542.008  545.502  548.997  552.488
#>  [73]  555.979  559.469  562.955  566.442  569.928  573.411  576.894  580.376
#>  [81]  583.854  587.331  590.811  594.286  597.759  601.231  604.705  608.174
#>  [89]  611.642  615.109  618.574  622.038  625.503  628.966  632.426  635.884
#>  [97]  639.342  642.798  646.252  649.706  653.158  656.608  660.058  663.506
#> [105]  666.953  670.398  673.842  677.285  680.727  684.165  687.602  691.040
#> [113]  694.476  697.911  701.345  704.775  708.204  711.634  715.063  718.490
#> [121]  721.912  725.336  728.760  732.180  735.599  739.018  742.434  745.849
#> [129]  749.265  752.675  756.088  759.498  762.906  766.313  769.719  773.124
#> [137]  776.527  779.929  783.330  786.729  790.127  793.524  796.919  800.312
#> [145]  803.705  807.097  810.485  813.874  817.262  820.647  824.032  827.416
#> [153]  830.797  834.179  837.557  840.936  844.312  847.688  851.061  854.433
#> [161]  857.806  861.175  864.543  867.910  871.278  874.642  878.005  881.367
#> [169]  884.728  888.087  891.445  894.802  898.157  901.511  904.864  908.216
#> [177]  911.567  914.914  918.262  921.609  924.954  928.296  931.639  934.981
#> [185]  938.320  941.659  944.995  948.332  951.665  954.999  958.330  961.660
#> [193]  964.991  968.318  971.644  974.971  978.295  981.617  984.940  988.258
#> [201]  991.578  994.896  998.212 1001.530 1004.840 1008.150 1011.460 1014.770
#> [209] 1018.080 1021.390 1024.700 1028.000 1031.300 1034.610 1037.910 1041.210
#> [217] 1044.510 1047.800 1051.100 1054.390 1057.680 1060.980 1064.270 1067.560
#> [225] 1070.840 1074.130 1077.420 1080.700 1083.980 1087.270 1090.550 1093.830
#> [233] 1097.100 1100.380 1103.650 1106.930 1110.200 1113.470 1116.740 1120.010
#> [241] 1123.280 1126.550 1129.810 1133.080 1136.340 1139.600 1142.860 1146.120
#> [249] 1149.370 1152.630 1155.890 1159.140 1162.390 1165.640 1168.890 1172.140
#> [257] 1175.390 1178.640 1181.880 1185.120 1188.370 1191.610 1194.850 1198.090
#> [265] 1201.320 1204.560 1207.790 1211.030 1214.260 1217.490 1220.720 1223.950
#> [273] 1227.180 1230.400 1233.630 1236.850 1240.070 1243.300 1246.520 1249.740
#> [281] 1252.950 1256.170 1259.380 1262.600 1265.810 1269.020 1272.230 1275.440
#> [289] 1278.650 1281.850 1285.060 1288.260 1291.460 1294.670 1297.870 1301.070
#> [297] 1304.260 1307.460 1310.660 1313.850 1317.040 1320.230 1323.420 1326.610
#> [305] 1329.800 1332.990 1336.170 1339.360 1342.540 1345.720 1348.900 1352.080
#> [313] 1355.260 1358.440 1361.610 1364.790 1367.960 1371.130 1374.300 1377.470
#> [321] 1380.640 1383.810 1386.970 1390.140 1393.300 1396.470 1399.630 1402.790
#> [329] 1405.950 1409.100 1412.260 1415.410 1418.570 1421.720 1424.870 1428.020
#> [337] 1431.170 1434.320 1437.470 1440.610 1443.760 1446.900 1450.040 1453.180
#> [345] 1456.320 1459.460 1462.590 1465.730 1468.870 1472.000 1475.130 1478.260
#> [353] 1481.400 1484.520 1487.650 1490.780 1493.900 1497.030 1500.150 1503.270
#> [361] 1506.390 1509.510 1512.630 1515.740 1518.860 1521.970 1525.090 1528.200
#> [369] 1531.310 1534.420 1537.530 1540.640 1543.740 1546.850 1549.950 1553.050
#> [377] 1556.160 1559.260 1562.360 1565.450 1568.550 1571.650 1574.740 1577.830
#> [385] 1580.920 1584.020 1587.110 1590.190 1593.280 1596.370 1599.450 1602.540
#> [393] 1605.620 1608.700 1611.780 1614.860 1617.940 1621.020 1624.090 1627.170
#> [401] 1630.240 1633.310 1636.380 1639.450 1642.520 1645.590 1648.650 1651.720
#> [409] 1654.780 1657.850 1660.910 1663.970 1667.030 1670.090 1673.150 1676.200
#> [417] 1679.260 1682.310 1685.360 1688.410 1691.460 1694.510 1697.560 1700.610
#> [425] 1703.650 1706.700 1709.740 1712.780 1715.830 1718.860 1721.900 1724.940
#> [433] 1727.980 1731.010 1734.050 1737.080 1740.110 1743.140 1746.170 1749.200
#> [441] 1752.230 1755.250 1758.280 1761.300 1764.330 1767.350 1770.370 1773.390
#> [449] 1776.410 1779.420 1782.440 1785.460 1788.470 1791.480 1794.490 1797.500
#> [457] 1800.510 1803.520 1806.530 1809.530 1812.540 1815.540 1818.540 1821.540
#> [465] 1824.550 1827.550 1830.540 1833.540 1836.540 1839.530 1842.520 1845.510
#> [473] 1848.510 1851.500 1854.490 1857.480 1860.460 1863.450 1866.430 1869.410
#> [481] 1872.400 1875.380 1878.360 1881.340 1884.320 1887.290 1890.270 1893.240
#> [489] 1896.220 1899.190 1902.160 1905.130 1908.100 1911.070 1914.030 1917.000
#> [497] 1919.960 1922.930 1925.890 1928.850 1931.810 1934.770 1937.430 1940.400
#> [505] 1943.360 1946.330 1949.300 1952.270 1955.230 1958.190 1961.160 1964.120
#> [513] 1967.080 1970.040 1972.990 1975.950 1978.910 1981.860 1984.820 1987.770
#> [521] 1990.720 1993.670 1996.620 1999.570 2002.520 2005.460 2008.410 2011.350
#> [529] 2014.290 2017.240 2020.180 2023.120 2026.050 2028.990 2031.930 2034.870
#> [537] 2037.800 2040.730 2043.660 2046.600 2049.520 2052.450 2055.380 2058.310
#> [545] 2061.230 2064.160 2067.080 2070.000 2072.930 2075.850 2078.770 2081.680
#> [553] 2084.600 2087.520 2090.430 2093.340 2096.260 2099.170 2102.080 2104.990
#> [561] 2107.900 2110.810 2113.710 2116.620 2119.520 2122.430 2125.330 2128.230
#> [569] 2131.130 2134.030 2136.930 2139.820 2142.720 2145.610 2148.510 2151.400
#> [577] 2154.290 2157.180 2160.070 2162.960 2165.850 2168.730 2171.620 2174.500
#> [585] 2177.390 2180.270 2183.150 2186.030 2188.910 2191.790 2194.670 2197.540
#> [593] 2200.420 2203.290 2206.160 2209.030 2211.900 2214.770 2217.640 2220.510
#> [601] 2223.380 2226.240 2229.110 2231.970 2234.830 2237.690 2240.550 2243.410
#> [609] 2246.270 2249.130 2251.980 2254.840 2257.690 2260.540 2263.390 2266.250
#> [617] 2269.100 2271.940 2274.790 2277.640 2280.490 2283.330 2286.170 2289.020
#> [625] 2291.860 2294.700 2297.540 2300.380 2303.210 2306.050 2308.890 2311.720
#> [633] 2314.550 2317.390 2320.220 2323.050 2325.880 2328.700 2331.530 2334.360
#> [641] 2337.180 2340.010 2342.830 2345.650 2348.470 2351.290 2354.110 2356.930
#> [649] 2359.750 2362.560 2365.380 2368.190 2371.000 2373.820 2376.630 2379.440
#> [657] 2382.250 2385.050 2387.860 2390.670 2393.470 2396.280 2399.080 2401.880
#> [665] 2404.680 2407.480 2410.280 2413.080 2415.870 2418.670 2421.460 2424.260
#> [673] 2427.050 2429.840 2432.630 2435.420 2438.210 2441.000 2443.790 2446.570
#> [681] 2449.360 2452.140 2454.920 2457.700 2460.480 2463.260 2466.040 2468.820
#> [689] 2471.600 2474.370 2477.150 2479.920 2482.690 2485.470 2488.240 2491.010
#> [697] 2493.770 2496.540 2499.310 2502.070 2504.840 2507.600 2510.370 2513.130
#> [705] 2515.890 2518.650 2521.410 2524.160 2526.920 2529.680 2532.430 2535.190
#> [713] 2537.940 2540.690 2543.440 2546.190 2548.940 2551.690 2554.440 2557.180
#> [721] 2559.930 2562.670 2565.420 2568.160 2570.900 2573.640 2576.380 2579.110
#> [729] 2581.850 2584.590 2587.320 2590.060 2592.790 2595.520 2598.250 2600.980
#> [737] 2603.720 2606.440 2609.170 2611.900 2614.620 2617.350 2620.070 2622.790
#> [745] 2625.510 2628.230 2630.950 2633.670 2636.390 2639.110 2641.820 2644.540
#> [753] 2647.250 2649.960 2652.680 2655.390 2658.100 2660.810 2663.510 2666.220
#> [761] 2668.930 2671.630 2674.340 2677.040 2679.740 2682.440 2685.140 2687.840
#> [769] 2690.540 2693.240 2695.940 2698.630 2701.330 2704.020 2706.710 2709.410
#> [777] 2712.100 2714.790 2717.470 2720.160 2722.850 2725.530 2728.220 2730.900
#> [785] 2733.590 2736.270 2738.950 2741.630 2744.310 2746.990 2749.670 2752.340
#> [793] 2755.020 2757.690 2760.370 2763.040 2765.710 2768.380 2771.050 2773.720
#> [801] 2776.390 2779.060 2781.720 2784.390 2787.050 2789.710 2792.380 2795.040
#> [809] 2797.700 2800.360 2803.020 2805.680 2808.330 2810.990 2813.640 2816.300
#> [817] 2818.950 2821.600 2824.250 2826.900 2829.550 2832.200 2834.850 2837.490
#> [825] 2840.140 2842.780 2845.430 2848.070 2850.710 2853.350 2855.990 2858.630
#> [833] 2861.270 2863.910 2866.540 2869.180 2871.810 2874.450 2877.080 2879.710
#> [841] 2882.340 2884.970 2887.600 2890.230 2892.860 2895.480 2898.110 2900.730
#> [849] 2903.350 2905.970 2908.600 2911.220 2913.840 2916.460 2919.070 2921.690
#> [857] 2924.310 2926.920 2929.540 2932.150 2934.760 2937.370 2939.980 2942.590
#> [865] 2945.200 2947.810 2950.420 2953.020 2955.630 2958.230 2960.830 2963.440
#> [873] 2966.040 2968.640 2971.240 2973.830 2976.430 2979.030 2981.630 2984.220
#> [881] 2986.810 2989.410 2992.000 2994.590 2997.180 2999.770 3002.360 3004.950
#> [889] 3007.530 3010.120 3012.700 3015.290 3017.870 3020.450 3023.040 3025.620
#> [897] 3028.200 3030.770 3033.350 3035.930 3038.500 3041.080 3043.650 3046.230
#> [905] 3048.800 3051.370 3053.940 3056.510 3059.080 3061.650 3064.210 3066.780
#> [913] 3069.350 3071.910 3074.470 3077.040 3079.600 3082.160 3084.720 3087.280
#> [921] 3089.830 3092.390 3094.950 3097.500 3100.060 3102.610 3105.160 3107.720
#> [929] 3110.270 3112.820 3115.370 3117.910 3120.460 3123.010 3125.560 3128.100
#> [937] 3130.640 3133.190 3135.730 3138.270 3140.810 3143.350 3145.890 3148.430
#> [945] 3150.960 3153.500 3156.030 3158.570 3161.100 3163.640 3166.170 3168.700
#> [953] 3171.230 3173.760 3175.300 3177.840 3180.380 3182.910 3185.450 3187.990
#> [961] 3190.520 3193.060 3195.590 3198.120
raman_hdpe$spectra # look at just the spectral intensities data.table.
#>      intensity
#>          <int>
#>   1:        26
#>   2:        50
#>   3:        48
#>   4:        45
#>   5:        46
#>  ---          
#> 960:        71
#> 961:        71
#> 962:        75
#> 963:        75
#> 964:        67
raman_hdpe$metadata # look at just the metadata of the spectra.
#>        x     y  user_name spectrum_type spectrum_identity      organization
#>    <int> <int>     <char>        <char>            <char>            <char>
#> 1:     1     1 Win Cowger         Raman              HDPE Horiba Scientific
#>     license                                                        session_id
#>      <char>                                                            <char>
#> 1: CC BY-NC 5728ddde4f649fd71f6f487fc5ad8d80/dc85257201307a131e71d9ec24aaccbf
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b

# Creating a list and transforming to OpenSpecy
as_OpenSpecy(list(wavenumber = raman_hdpe$wavenumber,
                  spectra = raman_hdpe$spectra,
                  metadata = raman_hdpe$metadata[,-c("x", "y")]))
#>      wavenumber intensity
#>           <num>     <int>
#>   1:    301.040        26
#>   2:    304.632        50
#>   3:    308.221        48
#>   4:    311.810        45
#>   5:    315.398        46
#>  ---                     
#> 960:   3187.990        71
#> 961:   3190.520        71
#> 962:   3193.060        75
#> 963:   3195.590        75
#> 964:   3198.120        67
#> 
#> $metadata
#>        x     y  user_name spectrum_type spectrum_identity      organization
#>    <int> <int>     <char>        <char>            <char>            <char>
#> 1:     1     1 Win Cowger         Raman              HDPE Horiba Scientific
#>     license                                                        session_id
#>      <char>                                                            <char>
#> 1: CC BY-NC 5728ddde4f649fd71f6f487fc5ad8d80/dc85257201307a131e71d9ec24aaccbf
#>                             file_id    col_id
#>                              <char>    <char>
#> 1: cb06ce2846b119d932fb6696479a445b intensity

# If you try to produce an OpenSpecy using an OpenSpecy it will just return
# the same object.
as_OpenSpecy(raman_hdpe)
#>      wavenumber intensity
#>           <num>     <int>
#>   1:    301.040        26
#>   2:    304.632        50
#>   3:    308.221        48
#>   4:    311.810        45
#>   5:    315.398        46
#>  ---                     
#> 960:   3187.990        71
#> 961:   3190.520        71
#> 962:   3193.060        75
#> 963:   3195.590        75
#> 964:   3198.120        67
#> 
#> $metadata
#>        x     y  user_name spectrum_type spectrum_identity      organization
#>    <int> <int>     <char>        <char>            <char>            <char>
#> 1:     1     1 Win Cowger         Raman              HDPE Horiba Scientific
#>     license                                                        session_id
#>      <char>                                                            <char>
#> 1: CC BY-NC 5728ddde4f649fd71f6f487fc5ad8d80/dc85257201307a131e71d9ec24aaccbf
#>                             file_id
#>                              <char>
#> 1: cb06ce2846b119d932fb6696479a445b

# Creating an OpenSpecy from a data.frame
as_OpenSpecy(x = data.frame(wavenumber = raman_hdpe$wavenumber,
                            spectra = raman_hdpe$spectra$intensity))
#> Ambiguous column names: taking 'spectra' data from all but the 'wavenumber' column; use 'colnames' to supply user-defined columnsFALSE
#>      wavenumber spectra
#>           <num>   <int>
#>   1:    301.040      26
#>   2:    304.632      50
#>   3:    308.221      48
#>   4:    311.810      45
#>   5:    315.398      46
#>  ---                   
#> 960:   3187.990      71
#> 961:   3190.520      71
#> 962:   3193.060      75
#> 963:   3195.590      75
#> 964:   3198.120      67
#> 
#> $metadata
#>        x     y  license  col_id                          file_id
#>    <int> <int>   <char>  <char>                           <char>
#> 1:     1     1 CC BY-NC spectra bf49db07d97b169635b8f8e17311cdbc

# Test that the spectrum is formatted as an OpenSpecy object.
is_OpenSpecy(raman_hdpe)
#> [1] TRUE
is_OpenSpecy(raman_hdpe$spectra)
#> [1] FALSE
```
