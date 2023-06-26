functions {
    // function for determining phase
  int phase(real t){
    int i;
    
    
    if (t < 26){
      i = 1;
    }
    else if (t < 43){
      i = 2;
    }
    else if (t < 55){
      i = 3;
    }
    else if (t < 75){
      i = 4;
    }
    else {
      i = 5;
    }
    return i;
  }
  
  // diff function
  vector diff(vector column){
    vector[151-1] out;
    for (i in 1:(151-1)){
      out[i] = column[i+1] - column[i];
    }
    return out;
  }
  // x01 <- 1 / (1 + exp(-x))
  real expit(real x){
    real out = 1.0 / (1.0 + exp(-x));
    return out;
  } 


  real signal_g (real t){
    real out;
    if (t < 26){
      out = 0.45;
    }
    else if (t < 43){
      out = 0.5;
    }
    else if (t < 55){
      out = 0.54;
    }
    else if (t < 75){
      out = 0.72;
    }
    else {
      out = 0.8;
    }
    return out;
  }
  
  real signal_n1 (real t){
    real out;
    row_vector[151] dat;
    real i;
    dat = [1802257,1791605,1780476,1769473,1763297,1755031, 
    1744370,1732531,1718811,1705762,1694291,1687391,
    1683926,1675933,1666756,1657842,1646503,1637796,
    1633439,1627287,1620242,1612868,1605880,1597826,
    1591580,1588507,1583125,1576773,1570537,1565195,
    1558945,1554252,1551985,1547710,1543151,1537669,
    1532396,1527154,1523661,1521974,1518605,1515002,
    1511081,1507609,1504059,1502089,1500912,1500208,
    1497295,1494268,1491447,1488766,1487008,1486108,
    1484106,1481782,1479642,1477775,1475871,1474523,
    1473904,1472599,1470959,1469269,1467719,1466119,
    1464868,1464319,1463123,1461660,1460245,1458945,
    1457611,1456502,1456020,1454923,1453798,1452749,
    1451702,1450518,1449486,1449033,1448319,1447274,
    1446247,1445199,1444048,1443113,1442658,1441913,
    1441011,1439978,1438950,1437755,1436681,1436271,
    1435333,1434159,1432964,
    1431475,1431103,1430159,
    1429794,1429089,1427849,
    1426600,1425309,1423944,
    1422856,1422314,1421255,
    1420039,1418759,1417369,
    1416236,1415407,1415053,
    1414143,1413084,1411915,
    1410451,1409318,1408406,
    1408035,1407104,1405896,
    1404776,1403617,1402536,
    1401742,1401333,1400586,
    1399418,1398480,1397396,
    1396460,1395866,1395579,
    1395451,1394545,1393720,
    1392668,1391713,1390910,
    1390567,1389680,1388781,
    1387958,1387017,1386125,
    1385498];
    

    i = floor(t);
    if (i <= 1){
        out = dat[1];
    }
    else{
    for (out_i in 1:151) {
      if ( (out_i >= i) && (out_i < i+1) ){
        out = dat[out_i];
      }
      else{
        // continue;
        int a;
      }
    }
    }
    return out;
  }
  
  real signal_n2 (real t){
    real out;
    row_vector[151] dat;
    real i;
    dat = [804540, 806153, 809114, 812651, 813972, 814038, 814523,
    815167, 816121, 815754, 816378, 816218, 816047, 815176,
    815148, 813495, 810425, 804305, 800031, 795336, 786938,
    778690, 769579, 757609, 742344, 734895, 728204, 719048,
    710535, 702242, 690977, 678792, 672879, 667394, 660973,
    654163, 645972, 637187, 627143, 622276, 618396, 611915,
    605510, 599579, 592707, 586326, 583635, 582549, 578518,
    574087, 569883, 565192, 559292, 557405, 554926, 552241,
    549846, 547026, 543402, 538846, 537495, 535844, 533702,
    531320, 528570, 525320, 521484, 519705, 517684, 514808,
    511795, 509474, 506315, 503419, 502216, 501171, 499614, 
    497881, 496466, 494591, 492037, 491247, 490421, 489335,
    488051, 486926, 485639, 483993, 483452, 482953, 482199,
    481279, 480478, 479472, 478174, 477713,
    477350, 476670, 475952, 475082, 474859, 473894,
    473787, 473679, 473292, 472666, 472227, 471494,
 470703, 470517, 470262, 469853, 469419, 469205,
 468580, 467866, 467694, 467467, 466896, 466647,
 466495, 466171, 465780, 465694, 465620, 465325,
 465065, 464815, 464507, 464050, 463890, 463640,
 463451, 462961, 462611, 462188, 461875, 461692,
 461655, 461377, 461096, 460834, 460446, 460000,
 459722, 459508, 459246, 458942, 458586, 458127,
 457643];
    

    i = floor(t);
    if (i <= 1){
        out = dat[1];
    }
    else{
    for (out_i in 1:151) {
      if ( (out_i >= i) && (out_i < i+1) ){
        out = dat[out_i];
      }
      else{
        // continue;
        int a;
      }
    }
    }
    return out;
  }



  real signal_n3 (real t){
    real out;
    row_vector[151] dat;
    real i;
    dat = [7212511, 7047274, 6890774, 6783111, 6705333, 6588480, 6459829, 6330184,
 6206623, 6089480, 6005315, 5959235, 5930695, 5853567, 5776498, 5704126,
5639707, 5599153, 5576874, 5538499, 5497514, 5456959, 5418402, 5379614,
 5360640, 5348402, 5322952, 5298445, 5277385, 5262137, 5241026, 5228434,
 5223027, 5209571, 5197569, 5183979, 5173239, 5161024, 5154620, 5146813,
 5141108, 5135235, 5129398, 5123743, 5107343, 5092783, 5083999, 5078768,
5067399, 5058060, 5049719, 5039820, 5029766, 5025731, 5021486, 5016183,
 5011337, 5007171, 5001606, 4997560, 4996007, 4994290, 4991030, 4987880,
 4984866, 4981184, 4977893, 4976697, 4973598, 4970611, 4968003, 4965064,
 4961567, 4959233, 4958098, 4955956, 4953265, 4951346, 4948897, 4946359,
 4944683, 4943896, 4942546, 4940658, 4938305, 4935530, 4932316, 4929924,
 4928557, 4926651, 4923855,4920849, 4917783,4913953,4911014,4909434,
 4906872,4903515,4900247,4895730, 4894567, 4892307,
 4891489, 4889141, 4885860, 4882728, 4879957, 4877050,
 4875168, 4874161, 4871927, 4869350, 4866840, 4864198,
 4861690, 4860267, 4859696, 4858140, 4856021, 4854061,
 4852469, 4850573, 4849550, 4849124, 4847737, 4846256,
 4844865, 4843585, 4842212, 4841412, 4840944, 4839997,
 4838888, 4837950, 4836935, 4835704, 4835137, 4834838,
 4834531, 4833606, 4832655, 4831692, 4830743, 4830255,
 4830113, 4829418, 4828538, 4827862, 4827128, 4826478,
 4826208];
    

    i = floor(t);
    if (i <= 1){
        out = dat[1];
    }
    else{
    for (out_i in 1:151) {
      if ( (out_i >= i) && (out_i < i+1) ){
        out = dat[out_i];
      }
      else{
        // continue;
        int a;
      }
    }
    }
    return out;
  }
  
  
    real signal_n4 (real t){
    real out;
    row_vector[151] dat;
    real i;
    dat = [ 4232672, 4406948, 4571616, 4686745, 4769378, 4894431, 5033258, 5174098,
            5310425, 5440984, 5535996, 5589136, 5621312, 5707304, 5793578, 5876517,
            5955345, 6010726, 6041636, 6090858, 6147286, 6203463, 6258119, 6316931,
            6357416, 6380176, 6417699, 6457714, 6493523, 6522406, 6561032, 6590502,
            6604089, 6627305, 6650287, 6676169, 6700373, 6726615, 6746556, 6760917,
            6773871, 6789828, 6805991, 6821049, 6847871, 6870782, 6883434, 6890455,
            6908768, 6925565, 6940931, 6958202, 6975914, 6982736, 6991462, 7001774,
            7011155, 7020008, 7031101, 7041051, 7044574, 7049247, 7056289, 7063511,
            7070825, 7079357, 7087735, 7091259, 7097575, 7104901, 7111937, 7118497,
            7126487, 7132826, 7135646, 7139930, 7145303, 7150004, 7154915, 7160512,
            7165774, 7167804, 7170694, 7174713, 7179377, 7184325, 7189977, 7194950,
            7197313, 7200463, 7204915,7209874, 7214769, 7220800, 7226111, 7228562,
 7232425, 7237636, 7242817, 7249693, 7251451, 7255620,
 7256910, 7260071, 7264979, 7269986, 7274487, 7279492,
 7283253, 7284988, 7288536, 7292738, 7296962, 7301208,
 7305474, 7308440, 7309537, 7312230, 7315979, 7319357,
 7322565, 7325918, 7328244, 7329127, 7331519, 7334503,
 7337274, 7339963, 7342725, 7344776, 7345813, 7347757,
 7350223, 7352589, 7355038, 7357628, 7359102, 7359871,
 7360343, 7362452, 7364509, 7366786, 7369078, 7370815,
 7371578, 7373374, 7375415, 7377218, 7379249, 7381250,
 7382631];


    i = floor(t);
    if (i <= 1){
        out = dat[1];
    }
    else{
    for (out_i in 1:151) {
      if ( (out_i >= i) && (out_i < i+1) ){
        out = dat[out_i];
      }
      else{
        // continue;
        int a;
      }
    }
    }
    return out;
  }
  
  
  
      real signal_v1 (real t){
    real out;
    row_vector[151] dat;
    real i;
    dat = [10652, 11129, 11003, 6176, 8266, 10661, 11839, 13720, 13049, 11471, 6900,
          3465, 7993, 9177, 8914, 11339,  8707 , 4357 , 6152, 7045, 7374,  6988,
          8054,6246,3073,5382,6352,6236,5342,6250,4693,2267,4275,
          4559,5482,5273,5242,3493,1687,3369,3603,3921,3472,3550,
          1970,1177, 704,2913,3027,2821,2681,1758, 900,2002,2324,
          2140,1867,1904,1348, 619,1305,1640,1690,1550,1600,1251,
          549,1196,1463,1415,1300,1334,1109, 482,1097,1125,1049,
          1047,1184,1032, 453, 714,1045,1027,1048,1151, 935, 455,
          745, 902,1033, 1028,  1195,  1074,   410,   938,
 1174,  1195,  1489,   372,   944 ,  365,   705,  1240,
  1249,  1291,  1365,  1088,   542,  1059,  1216,  1280,
  1390,  1133,   829,  354,   910 , 1059,  1169,  1464,
  1133,   912,   371,   931,  1208,  1120,  1159,  1081,
   794,   409,   747,  1168,   938,  1084,  936 ,  594,
   287,   128,   906,   825,  1052,   955,   803,   343,
   887,   899,   823,   941,   892,   627, 265];


    i = floor(t);
    if (i <= 1){
        out = dat[1];
    }
    else{
    for (out_i in 1:151) {
      if ( (out_i >= i) && (out_i < i+1) ){
        out = dat[out_i];
      }
      else{
        // continue;
        int a;
      }
    }
    }
    return out;
  }
  
  
  
  
  real signal_v2 (real t){
    real out;
    row_vector[151] dat;
    real i;
    dat = [ 9039,8168,7466,4855,8200,10176,11195,12766,13416,10847,7060,
           3636,8864,9205,10567,14409,14827,8631,10847,15443,15622,16099,
            20024,21511,10522,12073,15508,14749,13635,17515,16878,8180,9760,
            10980,12292,13464,14027,13537,6554,7249,10084,10326,9403,10422,
            8351,3868,1790,6944,7458,7025,7372,7658,2787,4481,5009,
            4535,4687,5528,5904,1970,2956,3782,4072,4300,4850,5087,
            2328,3217,4339,4428,3621,4493,4005,1685,2142,2682,2782,
            2462,3059,3586,1243,1540,2131,2311,2173,2438,2581, 996,
            1244,1656,1953,1829,  2201,  2372,   871,  1301 , 1854,  1913,
            2359,   595,1909,   472,   813,  1627,  1875,  1730,  2098,  1879,   728,  1314,
  1625,  1714,  1604,  1758,  1543,   526,  1137,  1630,  1418,  1616,
  1457,  1303,   457,  1005,  1503,  1380,  1409 , 1389,  1251  , 569,
  997 , 1357,  1428 , 1434,  1359 ,  907,   470 ,  165,  1184 , 1106,
  1314,  1343,  1249,   621,  1101 , 1161,  1127,  1297,  1351,  1111,
   460];


    i = floor(t);
    if (i <= 1){
        out = dat[1];
    }
    else{
    for (out_i in 1:151) {
      if ( (out_i >= i) && (out_i < i+1) ){
        out = dat[out_i];
      }
      else{
        // continue;
        int a;
      }
    }
    }
    return out;
  }
  
  
  
    real signal_v3 (real t){
    real out;
    row_vector[151] dat;
    real i;
    dat = [174276 ,164668 ,115129,82633 ,125053 ,138827 ,140840 ,136327 ,130559,95012,
53140 ,32176 ,85992 ,86274 ,82939 ,78828 ,55381 ,30910 ,49222 ,56428,
56177 ,54656 ,58812 ,40485 ,22760 ,37523 ,40015 ,35809 ,28883 ,38626,
29470 ,13587 ,23216 ,22982 ,25882 ,24204 ,26242 ,19941 ,14361 ,12954,
15957 ,16163 ,15058 ,26822 ,22911 ,12652 , 7021 ,18313 ,16797 ,15366,
17271 ,17712 , 6822 , 8726 ,10312 , 9381 , 8853 ,11093 , 9950 , 3523,
 4673 , 7042 , 7222 , 7314 , 8532 , 8378 , 3524 , 6316 , 7326 , 7036,
 6560 , 7990 , 6339 , 2820 , 4284 , 5373 , 4701 , 4911 , 5597 , 5262,
 2030 , 2890 , 4019 , 4664 , 4948 , 5652 , 4973 , 2363 , 3150 , 4452,
 4959 , 4895,   6031,   5311, 2451,   3863,
    5211  , 5181 ,  6876,   1758,   4169,   1290,   3161,   4908,
   5007  , 4501,   5005 ,  3761,   1735  , 3548,   4202,   4224,
   4246 ,  4266,   2966 ,  1097,   2693,   3749,   3378,   3208,
   3353 ,  2326,    883 ,  2392,   2984,   2771,   2689,   2762,
   2051 ,  1037,   1944 ,  2466,   2366,   2449,   2590,   1474,
    769 ,   472,   2109 ,  2057,   2277,   2292,   1737,    763,
   1796,   2041,   1803 ,  2031,   2001,   1381,    623];


    i = floor(t);
    if (i <= 1){
        out = dat[1];
    }
    else{
    for (out_i in 1:151) {
      if ( (out_i >= i) && (out_i < i+1) ){
        out = dat[out_i];
      }
      else{
        // continue;
        int a;
      }
    }
    }
    return out;
  }
  

  
  
  real[] sir(real t, real[] y, real[] theta, 
             real[] x_r, int[] x_i) {
//  1
      
      real gg = signal_g(t);
      real nn1 = signal_n1(t);
      real nn2 = signal_n2(t);
      real nn3 = signal_n3(t);
      real nn4 = signal_n4(t);
      real vv1 = signal_v1(t);
      real vv2 = signal_v2(t);
      real vv3 = signal_v3(t);
      
      
      real E1 = y[1];
      real A1 = y[2];
      real P1 = y[3];
      real I1 = y[4];
      real R1 = y[5];
      real Q1 = y[6];
      real D1 = y[7];
      real RA1 = y[8];
      real Rprime1 = y[9];
      real Tprime1 = y[10];
      real T1 = y[11];
      
      
 // 2

      real E2 = y[12];
      real A2 = y[13];
      real P2 = y[14];
      real I2 = y[15];
      real R2 = y[16];
      real Q2 = y[17];
      real D2 = y[18];
      real RA2 = y[19];
      real Rprime2 = y[20];
      real Tprime2 = y[21];
      real T2 = y[22];
      
      
//  3

      real E3 = y[23];
      real A3 = y[24];
      real P3 = y[25];
      real I3 = y[26];
      real R3 = y[27];
      real Q3 = y[28];
      real D3 = y[29];
      real RA3 = y[30];
      real Rprime3 = y[31];
      real Tprime3 = y[32];
      real T3 = y[33];
   
      
      
      
//  4

      real E4 = y[34];
      real A4 = y[35];
      real P4 = y[36];
      real I4 = y[37];
      real R4 = y[38];
      real Q4 = y[39];
      real D4 = y[40];
      real RA4 = y[41];
      real Rprime4 = y[42];
      real Tprime4 = y[43];
      real T4 = y[44];
      
      real S1 = y[45];
      real S2 = y[46];
      real S3 = y[47];
      real S4 = y[48];
   
      
      
   

// unknown parameters

    int i = phase(t);
    real f_1 = theta[(i-1)*4 + 1];
    real f_2 = theta[(i-1)*4 + 2];
    real f_3 = theta[(i-1)*4 + 3];
    real f_4 = theta[(i-1)*4 + 4];
    
    real CAR = theta[20 + i];
    

    // fixed parameters


    real kappa_E = 1/1.42; 
    real kappa_A = 1/8.29; 
    real kappa_I_R = 1/6.87; 
    real kappa_I_Q = 1.0/1.0;  
    real kappa_Q_R = 1/5.87; 
  
    real alpha_1 = 0.003;
    real alpha_2 = 0.0019; 
    real alpha_3 = 0.0008; 
    real alpha_4 = 0.0007; 
  
  
    real kappa_T_prime = 1.0/(8.29 - 2); 
    real g = 0.45; 
  
    real kappa_P = 1.0/2; 
  
  
    real epsilon = 0.96; 
  
    real beta_11 = 2.4844 * 0.02;
    real beta_12 = 1.104594  * 0.02;
    real beta_13 = 6.379842 * 0.02;
    real beta_14 = 4.187418  * 0.02;

    real beta_21 = 2.781948 * 0.02;
    real beta_22 = 1.335314  * 0.02;
    real beta_23 = 6.37106 * 0.02;
    real beta_24 = 3.174107  * 0.02;
  
    real beta_31 = 1.874393 * 0.02;
    real beta_32 = 0.743218  * 0.02;
    real beta_33 = 5.700929 * 0.02;
    real beta_34 = 5.339735  * 0.02;
  
    real beta_41 = 0.939218 * 0.02;
    real beta_42 = 0.28268  * 0.02;
    real beta_43 = 4.076515 * 0.02;
    real beta_44 = 6.33165  * 0.02;


  

    real dS1 = (-S1/nn1*(beta_11*P1+beta_11*I1+0.2*beta_11*A1 + 
    beta_11*T1+beta_11*Tprime1) -
              S1/nn2*(beta_12*P2+beta_12*I2+0.2*beta_12*A2 + 
    beta_12*T2+beta_12*Tprime2) -
              S1/nn3*(beta_13*P3+beta_13*I3+0.2*beta_13*A3 + 
    beta_13*T3+beta_13*Tprime3) -
              S1/nn4*(beta_14*P4+beta_14*I4+0.2*beta_14*A4 +
    beta_14*T4+beta_14*Tprime4))*gg*1 - vv1;
    
    
    real dE1 = (S1/nn1*(beta_11*P1+beta_11*I1+0.2*beta_11*A1 + 
    beta_11*T1+beta_11*Tprime1) +
              S1/nn2*(beta_12*P2+beta_12*I2+0.2*beta_12*A2 + 
    beta_12*T2+beta_12*Tprime2) +
              S1/nn3*(beta_13*P3+beta_13*I3+0.2*beta_13*A3  + 
    beta_13*T3+beta_13*Tprime3) +
              S1/nn4*(beta_14*P4+beta_14*I4+0.2*beta_14*A4 +
    beta_14*T4+beta_14*Tprime4))*gg*1 - 
      (1 - f_1)*kappa_E * E1 - f_1 * E1 * kappa_E;
    
    
    real dA1 = f_1 * E1 * kappa_E - kappa_A *A1;
    
    
    real dP1 = (1-f_1) * kappa_E * E1 - kappa_P * CAR * P1 - (1 - CAR) * kappa_P * P1;
    
    
    real dTprime1 = (1 - CAR) * kappa_P * P1 - kappa_T_prime * Tprime1;
    
    real dRprime1 = kappa_T_prime * Tprime1;
    
    real dI1 = kappa_P * CAR * P1 - (alpha_1+ (1-alpha_1)*(1-epsilon)*kappa_I_Q + 
                                   (1-alpha_1)*epsilon*kappa_I_Q)*I1;
    
    real dT1 = (1-alpha_1)*(1-epsilon)*kappa_I_Q * I1 - kappa_Q_R * T1;
    
    real dR1 =  kappa_Q_R * T1+ kappa_Q_R *  Q1 ;
    
    
    real dQ1 = (1-alpha_1)*(kappa_I_Q * epsilon)*I1-kappa_Q_R *  Q1;
    
    
    real dD1 = alpha_1 * I1;
    
    
    real dRA1 = kappa_A *A1;
    
    
    
    //2
     real dS2 = (-S2/nn1*(beta_21*P1+beta_21*I1+0.2*beta_21*A1 + 
    beta_21*T1+beta_21*Tprime1) -
              S2/nn2*(beta_22*P2+beta_22*I2+0.2*beta_22*A2 + 
    beta_22*T2+beta_22*Tprime2) -
              S2/nn3*(beta_23*P3+beta_23*I3+0.2*beta_23*A3 + 
    beta_23*T3+beta_23*Tprime3) -
              S2/nn4*(beta_24*P4+beta_24*I4+0.2*beta_24*A4 +
    beta_24*T4+beta_24*Tprime4))*gg*1 + vv1 - vv2;
    
    
    real dE2 = (S2/nn1*(beta_21*P1+beta_21*I1+0.2*beta_21*A1 + 
    beta_21*T1+beta_21*Tprime1) +
              S2/nn2*(beta_22*P2+beta_22*I2+0.2*beta_22*A2 + 
    beta_22*T2+beta_22*Tprime2) +
              S2/nn3*(beta_23*P3+beta_23*I3+0.2*beta_23*A3  + 
    beta_23*T3+beta_23*Tprime3) +
              S2/nn4*(beta_24*P4+beta_24*I4+0.2*beta_24*A4 +
    beta_24*T4+beta_24*Tprime4))*gg*1 - 
      (1 - f_2)*kappa_E * E2 - f_2 * E2 * kappa_E;
    
    
    real dA2 = f_2 * E2 * kappa_E - kappa_A *A2;
    
    
    real dP2 = (1-f_2) * kappa_E * E2 - kappa_P * CAR * P2 - (1 - CAR) * kappa_P * P2;
    
    
    real dTprime2 = (1 - CAR) * kappa_P * P2 - kappa_T_prime * Tprime2;
    
    real dRprime2 = kappa_T_prime * Tprime2;
    
    real dI2 = kappa_P * CAR * P2 - (alpha_2+ (1-alpha_2)*(1-epsilon)*kappa_I_Q + 
                                   (1-alpha_2)*epsilon*kappa_I_Q)*I2;
    
    real dT2 = (1-alpha_2)*(1-epsilon)*kappa_I_Q * I2 - kappa_Q_R * T2;
    
    real dR2 =  kappa_Q_R * T2+ kappa_Q_R *  Q2 ;
    
    
    real dQ2 = (1-alpha_2)*(kappa_I_Q * epsilon)*I2-kappa_Q_R *  Q2;
    
    
    real dD2 = alpha_2 * I2;
    
    
    real dRA2 = kappa_A *A2;
    

    
    
    
    /////
    
    
      //3
     real dS3 = (-S3/nn1*(beta_31*P1+beta_31*I1+0.2*beta_31*A1 + 
    beta_31*T1+beta_31*Tprime1) -
              S3/nn2*(beta_32*P2+beta_32*I2+0.2*beta_32*A2 + 
    beta_32*T2+beta_32*Tprime2) -
              S3/nn3*(beta_33*P3+beta_33*I3+0.2*beta_33*A3 + 
    beta_33*T3+beta_33*Tprime3) -
              S3/nn4*(beta_34*P4+beta_34*I4+0.2*beta_34*A4 +
    beta_34*T4+beta_34*Tprime4))*gg*1 + vv2 - vv3;
    
    
    real dE3 = (S3/nn1*(beta_31*P1+beta_31*I1+0.2*beta_31*A1 + 
    beta_31*T1+beta_31*Tprime1) +
              S3/nn2*(beta_32*P2+beta_32*I2+0.2*beta_32*A2 + 
    beta_32*T2+beta_32*Tprime2) +
              S3/nn3*(beta_33*P3+beta_33*I3+0.2*beta_33*A3  + 
    beta_33*T3+beta_33*Tprime3) +
              S3/nn4*(beta_34*P4+beta_34*I4+0.2*beta_34*A4 +
    beta_34*T4+beta_34*Tprime4))*gg*1 - 
      (1 - f_3)*kappa_E * E3 - f_3 * E3 * kappa_E;
    
    
    real dA3 = f_3 * E3 * kappa_E - kappa_A *A3;
    
    
    real dP3 = (1-f_3) * kappa_E * E3 - kappa_P * CAR * P3 - (1 - CAR) * kappa_P * P3;
    
    
    real dTprime3 = (1 - CAR) * kappa_P * P3 - kappa_T_prime * Tprime3;
    
    real dRprime3 = kappa_T_prime * Tprime3;
    
    real dI3 = kappa_P * CAR * P3 - (alpha_3 + (1-alpha_3)*(1-epsilon)*kappa_I_Q + 
                                   (1-alpha_3)*epsilon*kappa_I_Q)*I3;
    
    real dT3 = (1-alpha_3)*(1-epsilon)*kappa_I_Q * I3 - kappa_Q_R * T3;
    
    real dR3 =  kappa_Q_R * T3+ kappa_Q_R *  Q3 ;
    
    
    real dQ3 = (1-alpha_3)*(kappa_I_Q * epsilon)*I3-kappa_Q_R *  Q3;
    
    
    real dD3 = alpha_3 * I3;
    
    
    real dRA3 = kappa_A *A3;
    
     //4
     real dS4 = (-S4/nn1*(beta_41*P1+beta_41*I1+0.2*beta_41*A1 + 
    beta_41*T1+beta_41*Tprime1) -
              S4/nn2*(beta_42*P2+beta_42*I2+0.2*beta_42*A2 + 
    beta_42*T2+beta_42*Tprime2) -
              S4/nn3*(beta_43*P3+beta_43*I3+0.2*beta_43*A3 + 
    beta_43*T3+beta_43*Tprime3) -
              S4/nn4*(beta_44*P4+beta_44*I4+0.2*beta_44*A4 +
    beta_44*T4+beta_44*Tprime4))*gg*1 +  vv3;
    
    
    real dE4 = (S4/nn1*(beta_41*P1+beta_41*I1+0.2*beta_41*A1 + 
    beta_41*T1+beta_41*Tprime1) +
              S4/nn2*(beta_42*P2+beta_42*I2+0.2*beta_42*A2 + 
    beta_42*T2+beta_42*Tprime2) +
              S4/nn3*(beta_43*P3+beta_43*I3+0.2*beta_43*A3  + 
    beta_43*T3+beta_43*Tprime3) +
              S4/nn4*(beta_44*P4+beta_44*I4+0.2*beta_44*A4 +
    beta_44*T4+beta_44*Tprime4))*gg*1 - 
      (1 - f_4)*kappa_E * E4 - f_4 * E4 * kappa_E;
    
    
    real dA4 = f_4 * E4 * kappa_E - kappa_A *A4;
    
    
    real dP4 = (1-f_4) * kappa_E * E4 - kappa_P * CAR * P4 - (1 - CAR) * kappa_P * P4;
    
    
    real dTprime4 = (1 - CAR) * kappa_P * P4 - kappa_T_prime * Tprime4;
    
    real dRprime4 = kappa_T_prime * Tprime4;
    
    real dI4 = kappa_P * CAR * P4 - (alpha_4 + (1-alpha_4)*(1-epsilon)*kappa_I_Q + 
                                   (1-alpha_4)*epsilon*kappa_I_Q)*I4;
    
    real dT4 = (1-alpha_4)*(1-epsilon)*kappa_I_Q * I4 - kappa_Q_R * T4;
    
    real dR4 =  kappa_Q_R * T4+ kappa_Q_R *  Q4;
    
    
    real dQ4 = (1-alpha_4)*(kappa_I_Q * epsilon)*I4-kappa_Q_R *  Q4;
    
    
    real dD4 = alpha_4 * I4;
    
    
    real dRA4 = kappa_A *A4;
    
   

  return {dE1, dA1, dP1, dI1, dR1, dQ1, dD1, dRA1, dRprime1, dTprime1, dT1,
         dE2, dA2, dP2, dI2, dR2, dQ2, dD2, dRA2, dRprime2, dTprime2, dT2,
         dE3, dA3, dP3, dI3, dR3, dQ3, dD3, dRA3,dRprime3, dTprime3, dT3,
         dE4, dA4, dP4, dI4, dR4, dQ4, dD4, dRA4,dRprime4, dTprime4, dT4,
         dS1, dS2, dS3, dS4};
}

}






// fix data 
data {
  int<lower=1> n_days;
  real y0[48];
  real t0;
  real ts[n_days];
  int cases[4,n_days-1];

  
  
  
}




transformed data {
  real x_r[0];
  int x_i[0];

}






parameters {

  
  real f_1_p1;
  real f_2_p1;
  real f_3_p1;
  real f_4_p1;
  
  real f_1_p2;
  real f_2_p2;
  real f_3_p2;
  real f_4_p2;
  
  real f_1_p3;
  real f_2_p3;
  real f_3_p3;
  real f_4_p3;
  
  real f_1_p4;
  real f_2_p4;
  real f_3_p4;
  real f_4_p4;
  
  real f_1_p5;
  real f_2_p5;
  real f_3_p5;
  real f_4_p5;
  
  
  real CAR_p1;
  real CAR_p2;
  real CAR_p3;
  real CAR_p4;
  real CAR_p5;
  
  real<lower=0> phi_inv_1_p1;
  real<lower=0> phi_inv_2_p1;
  real<lower=0> phi_inv_3_p1;
  real<lower=0> phi_inv_4_p1;
  
  real<lower=0> phi_inv_1_p2;
  real<lower=0> phi_inv_2_p2;
  real<lower=0> phi_inv_3_p2;
  real<lower=0> phi_inv_4_p2;
  
  
  real<lower=0> phi_inv_1_p3;
  real<lower=0> phi_inv_2_p3;
  real<lower=0> phi_inv_3_p3;
  real<lower=0> phi_inv_4_p3;
  
  real<lower=0> phi_inv_1_p4;
  real<lower=0> phi_inv_2_p4;
  real<lower=0> phi_inv_3_p4;
  real<lower=0> phi_inv_4_p4;
  
  
  real<lower=0> phi_inv_1_p5;
  real<lower=0> phi_inv_2_p5;
  real<lower=0> phi_inv_3_p5;
  real<lower=0> phi_inv_4_p5;
}


transformed parameters{
  real y[n_days, 48];
  vector[n_days-1] out_I1;
  vector[n_days-1] out_I2;
  vector[n_days-1] out_I3;
  vector[n_days-1] out_I4;
  
  real phi_1_p1 = 1. / phi_inv_1_p1;
  real phi_2_p1 = 1. / phi_inv_2_p1;
  real phi_3_p1 = 1. / phi_inv_3_p1;
  real phi_4_p1 = 1. / phi_inv_4_p1;
  
  real phi_1_p2 = 1. / phi_inv_1_p2;
  real phi_2_p2 = 1. / phi_inv_2_p2;
  real phi_3_p2 = 1. / phi_inv_3_p2;
  real phi_4_p2 = 1. / phi_inv_4_p2;
  
  real phi_1_p3 = 1. / phi_inv_1_p3;
  real phi_2_p3 = 1. / phi_inv_2_p3;
  real phi_3_p3 = 1. / phi_inv_3_p3;
  real phi_4_p3 = 1. / phi_inv_4_p3;
  
  
  real phi_1_p4 = 1. / phi_inv_1_p4;
  real phi_2_p4 = 1. / phi_inv_2_p4;
  real phi_3_p4 = 1. / phi_inv_3_p4;
  real phi_4_p4 = 1. / phi_inv_4_p4;
  
  real phi_1_p5 = 1. / phi_inv_1_p5;
  real phi_2_p5 = 1. / phi_inv_2_p5;
  real phi_3_p5 = 1. / phi_inv_3_p5;
  real phi_4_p5 = 1. / phi_inv_4_p5;
  
{
  real theta[25];
  
  theta[1] = expit(f_1_p1);
  theta[2] = expit(f_2_p1);
  theta[3] = expit(f_3_p1);
  theta[4] = expit(f_4_p1);
  
  theta[5] = expit(f_1_p2);
  theta[6] = expit(f_2_p2);
  theta[7] = expit(f_3_p2);
  theta[8] = expit(f_4_p2);
  
  theta[9] = expit(f_1_p3);
  theta[10] = expit(f_2_p3);
  theta[11] = expit(f_3_p3);
  theta[12] = expit(f_4_p3);
  
  theta[13] = expit(f_1_p4);
  theta[14] = expit(f_2_p4);
  theta[15] = expit(f_3_p4);
  theta[16] = expit(f_4_p4);
  
  theta[17] = expit(f_1_p5);
  theta[18] = expit(f_2_p5);
  theta[19] = expit(f_3_p5);
  theta[20] = expit(f_4_p5);
  
  theta[21] = expit(CAR_p1);
  theta[22] = expit(CAR_p2);
  theta[23] = expit(CAR_p3);
  theta[24] = expit(CAR_p4);
  theta[25] = expit(CAR_p5);


  y = integrate_ode_rk45(sir, y0, t0, ts, theta, x_r, x_i);
  
  out_I1 = diff(col(to_matrix(y), 4) + col(to_matrix(y), 5) + col(to_matrix(y), 6) + col(to_matrix(y), 7)+col(to_matrix(y), 11) );
  out_I2 = diff(col(to_matrix(y), 15) + col(to_matrix(y), 16) + col(to_matrix(y), 17) + col(to_matrix(y), 18)+col(to_matrix(y), 22));
  out_I3 = diff(col(to_matrix(y), 26) + col(to_matrix(y), 27) + col(to_matrix(y), 28) + col(to_matrix(y), 29)+col(to_matrix(y), 33));
  out_I4 =  diff(col(to_matrix(y), 37) + col(to_matrix(y), 38) + col(to_matrix(y), 39) + col(to_matrix(y), 40)+col(to_matrix(y), 44));

}
}




model {
  f_1_p1 ~ normal(-1, 2);
  f_2_p1 ~ normal(-0.5, 2);
  f_3_p1 ~ normal(1.5, 2);
  f_4_p1 ~ normal(2, 2);
  
  f_1_p2 ~ normal(-1, 2);
  f_2_p2 ~ normal(-0.5, 2);
  f_3_p2 ~ normal(1.5, 2);
  f_4_p2 ~ normal(2, 2);
  
  f_1_p3 ~ normal(-1, 2);
  f_2_p3 ~ normal(-0.5, 2);
  f_3_p3 ~ normal(1.5, 2);
  f_4_p3 ~ normal(2, 2);
  
  f_1_p4 ~ normal(-1, 2);
  f_2_p4 ~ normal(-0.5, 2);
  f_3_p4 ~ normal(1.5, 2);
  f_4_p4 ~ normal(2, 2);
  
  f_1_p5 ~ normal(-1, 2);
  f_2_p5 ~ normal(-0.5, 2);
  f_3_p5 ~ normal(1.5, 2);
  f_4_p5 ~ normal(2,2);
  
  // priors for CAR
  CAR_p1 ~ normal(-1.8364982, 1.31);
  CAR_p2 ~ normal(-1.9, 1.15);
  CAR_p3 ~ normal(-1.5, 1.31);
  CAR_p4 ~ normal(-1.09, 1.15);
  CAR_p5 ~ normal(-0.59, 1.31);

  phi_inv_1_p1 ~ exponential(5);
  phi_inv_2_p1 ~ exponential(5);
  phi_inv_3_p1 ~ exponential(5);
  phi_inv_4_p1 ~ exponential(5);
  
  phi_inv_1_p2 ~ exponential(5);
  phi_inv_2_p2 ~ exponential(5);
  phi_inv_3_p2 ~ exponential(5);
  phi_inv_4_p2 ~ exponential(5);
  
  phi_inv_1_p3 ~ exponential(5);
  phi_inv_2_p3 ~ exponential(5);
  phi_inv_3_p3 ~ exponential(5);
  phi_inv_4_p3 ~ exponential(5);
  
  phi_inv_1_p4 ~ exponential(5);
  phi_inv_2_p4 ~ exponential(5);
  phi_inv_3_p4 ~ exponential(5);
  phi_inv_4_p4 ~ exponential(5);
  
  phi_inv_1_p5 ~ exponential(5);
  phi_inv_2_p5 ~ exponential(5);
  phi_inv_3_p5 ~ exponential(5);
  phi_inv_4_p5 ~ exponential(5);
 
  

  

  
  
  cases[1, 1 : 24] ~ neg_binomial_2(out_I1[1   : 24],  phi_1_p1);
  cases[1, 25 : 41] ~ neg_binomial_2(out_I1[25 : 41], phi_1_p2);
  cases[1, 42 : 53] ~ neg_binomial_2(out_I1[42 : 53], phi_1_p3);
  cases[1, 54 : 73] ~ neg_binomial_2(out_I1[54 : 73], phi_1_p4);
  cases[1, 74 : 150] ~ neg_binomial_2(out_I1[74 : 150], phi_1_p5);
  
  cases[2, 1 : 24] ~ neg_binomial_2(out_I2[1   : 24],  phi_2_p1);
  cases[2, 25 : 41] ~ neg_binomial_2(out_I2[25 : 41], phi_2_p2);
  cases[2, 42 : 53] ~ neg_binomial_2(out_I2[42 : 53], phi_2_p3);
  cases[2, 54 : 73] ~ neg_binomial_2(out_I2[54 : 73], phi_2_p4);
  cases[2, 74 : 150] ~ neg_binomial_2(out_I2[74 : 150], phi_2_p5);
  
  cases[3, 1 : 24] ~ neg_binomial_2(out_I3[1   : 24],  phi_3_p1);
  cases[3, 25 : 41] ~ neg_binomial_2(out_I3[25 : 41], phi_3_p2);
  cases[3, 42 : 53] ~ neg_binomial_2(out_I3[42 : 53], phi_3_p3);
  cases[3, 54 : 73] ~ neg_binomial_2(out_I3[54 : 73], phi_3_p4);
  cases[3, 74 : 150] ~ neg_binomial_2(out_I3[74 : 150], phi_3_p5);
  
  
  cases[4, 1 : 24] ~ neg_binomial_2(out_I4[1   : 24],  phi_4_p1);
  cases[4, 25 : 41] ~ neg_binomial_2(out_I4[25 : 41], phi_4_p2);
  cases[4, 42 : 53] ~ neg_binomial_2(out_I4[42 : 53], phi_4_p3);
  cases[4, 54 : 73] ~ neg_binomial_2(out_I4[54 : 73], phi_4_p4);
  cases[4, 74 : 150] ~ neg_binomial_2(out_I4[74 : 150], phi_4_p5);
  
 
}


generated quantities {
  real pred_cases[4,n_days-1];
  
  pred_cases[1, 1 : 24] = neg_binomial_2_rng(out_I1[1   :24]+ 1e-5,  phi_1_p1);
  pred_cases[1, 25 : 41] = neg_binomial_2_rng(out_I1[25 : 41]+ 1e-5, phi_1_p2);
  pred_cases[1, 42 : 53] = neg_binomial_2_rng(out_I1[42 : 53]+ 1e-5, phi_1_p3);
  pred_cases[1, 54 : 73] = neg_binomial_2_rng(out_I1[54 : 73]+ 1e-5, phi_1_p4);
  pred_cases[1, 74 : 150] = neg_binomial_2_rng(out_I1[74 : 150]+ 1e-5, phi_1_p5);

  pred_cases[2, 1 : 24] = neg_binomial_2_rng(out_I2[1   :24]+ 1e-5,  phi_2_p1);
  pred_cases[2, 25 : 41] = neg_binomial_2_rng(out_I2[25 : 41]+ 1e-5, phi_2_p2);
  pred_cases[2, 42 : 53] = neg_binomial_2_rng(out_I2[42 : 53]+ 1e-5, phi_2_p3);
  pred_cases[2, 54 : 73] = neg_binomial_2_rng(out_I2[54 : 73]+ 1e-5, phi_2_p4);
  pred_cases[2, 74 : 150] = neg_binomial_2_rng(out_I2[74 : 150]+ 1e-5, phi_2_p5);
  
  pred_cases[3, 1 : 24] = neg_binomial_2_rng(out_I3[1   :24]+ 1e-5,  phi_3_p1);
  pred_cases[3, 25 : 41] = neg_binomial_2_rng(out_I3[25 : 41]+ 1e-5, phi_3_p2);
  pred_cases[3, 42 : 53] = neg_binomial_2_rng(out_I3[42 : 53]+ 1e-5, phi_3_p3);
  pred_cases[3, 54 : 73] = neg_binomial_2_rng(out_I3[54 : 73]+ 1e-5, phi_3_p4);
  pred_cases[3, 74 : 150] = neg_binomial_2_rng(out_I3[74 : 150]+ 1e-5, phi_3_p5);
  
  pred_cases[4, 1 : 24] = neg_binomial_2_rng(out_I4[1   :24]+ 1e-5,  phi_4_p1);
  pred_cases[4, 25 : 41] = neg_binomial_2_rng(out_I4[25 : 41]+ 1e-5, phi_4_p2);
  pred_cases[4, 42 : 53] = neg_binomial_2_rng(out_I4[42 : 53]+ 1e-5, phi_4_p3);
  pred_cases[4, 54 : 73] = neg_binomial_2_rng(out_I4[54 : 73]+ 1e-5, phi_4_p4);
  pred_cases[4, 74 : 150] = neg_binomial_2_rng(out_I4[74 : 150]+ 1e-5, phi_4_p5);
  
  
}
