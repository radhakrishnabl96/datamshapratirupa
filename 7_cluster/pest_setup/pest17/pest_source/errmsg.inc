      INTEGER MAXERR
      PARAMETER (MAXERR = 157)
      INTEGER ERRNO(MAXERR)
      CHARACTER*256 ERRFMT(MAXERR)

C --- Error numbers
      DATA ERRNO /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     + 21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
     + 42,43,44,45,46,47,48,50,52,64,65,66,67,68,69,70,71,72,73,74,75,
     + 78,81,82,84,88,90,92,93,94,95,96,97,98,101,102,103,105,106,107,
     + 108,109,110,112,113,114,115,116,117,118,123,124,125,126,127,128,
     + 129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
     + 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,
     + 159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,
     + 174,175,176,177,178,179,180,181,182,183,184,185,186,187,188/

C --- err# 1
      DATA ERRFMT(1) /'(1X,''Cannot open file '',A,''.'')'/
C --- err# 2
      DATA ERRFMT(2) /'(1X,''Error reading line '',A,'' of file '',A,
     +'': incorrect data type or data missing.'')'/
C --- err# 3
      DATA ERRFMT(3) /'(1X,''Unexpected end to file '',A,
     +'': data missing.'')'/
C --- err# 4
      DATA ERRFMT(4) /'(1X,''Cannot take log of zero or negative '',
     +''parameter value. Check dataset using PESTCHEK.'')'/
C --- err# 5
      DATA ERRFMT(5) /'(1X,''Command line argument should provide'',
     +'' filename base for current case.'')'/
C --- err# 6
      DATA ERRFMT(6) /'(1X,''Cannot open file '',A,'' for output.'')'/
C --- err# 7
      DATA ERRFMT(7) /'(1X,''Cannot reopen file '',A,
     +'' to continue PEST run record.'')'/
C --- err# 8
      DATA ERRFMT(8) /'(1X,''Error reading file PEST.HLT.'',
     +'' Dont know whether to continue execution or not.'')'/
C --- err# 9
      DATA ERRFMT(9) /'(1X,''Cannot allocate sufficient memory for'',
     +'' least squares optimization.'')'/
C --- err# 10
      DATA ERRFMT(10) /'(1X,''Error writing to file '',A,''.'')'/
C --- err# 11
      DATA ERRFMT(11) /'(1X,''Unrecognized parameter transformation/'',
     +''tied/fixed information (`'',A,''`):'',
     +'' line '',A,'' of file '',A,''.'')'/
C --- err# 12
      DATA ERRFMT(12) /'(1X,''Unrecognized parameter change limit '',
     +''type (`'',A,''`): line '',A,'' of file '',A,''.'')'/
C --- err# 13
      DATA ERRFMT(13) /'(1X,''Unrecognized parameter name (`'',
     +A,''`): line '',A,'' of file '',A,''.'')'/
C --- err# 14
      DATA ERRFMT(14) /'(1X,''Parameter cannot be tied to itself: '',
     +''line '',A,'' of file '',A,''.'')'/
C --- err# 15
      DATA ERRFMT(15) /'(1X,''Unknown parameter group (`'',A,
     +''`): line '',A,'' of file '',A,''.'')'/
C --- err# 16
      DATA ERRFMT(16) /'(1X,''Unrecognized derivative information (`'',
     +A,''`): line '',A,'' of file '',A,''.'')'/
C --- err# 17
      DATA ERRFMT(17) /'(1X,''Parameter `'',
     +A,''` assigned a scale value of zero:'',
     +'' line '',A,'' of file '',A,''.'')'/
C --- err# 18
      DATA ERRFMT(18) /'(1X,''Parameter group `none` is reserved for'',
     +'' some fixed and tied parameters.'',
     +'' No information can be provided for this group: line '',
     +A,'' of file '',A,''.'')'/
C --- err# 19
      DATA ERRFMT(19) /'(1X,''Cannot restart:'',
     +'' a PEST input file has been altered since saving restart '',
     +''data.'')'/
C --- err# 20
      DATA ERRFMT(20) /'(1X,''Only fixed or tied parameters can '',
     +''belong to parameter group `none`: line '',A,
     +'' of file '',A,''.'')'/
C --- err# 21
      DATA ERRFMT(21) /'(1X,''Improper command line syntax.'')'/
C --- err# 22
      DATA ERRFMT(22) /'(1X,''Cannot open file '',A,
     +'' to read restart data.'')'/
C --- err# 23
      DATA ERRFMT(23) /'(1X,''Error reading file '',A,
     +'' to obtain restart data. Have you altered any PEST '',
     +''input files since previous run was halted?'')'/
C --- err# 24
      DATA ERRFMT(24) /'(1X,''Unexpected end to file '',A,
     +'' holding restart data. Have you altered any PEST '',
     +''input files since previous run was halted?'')'/
C --- err# 25
      DATA ERRFMT(25) /'(1X,''Cannot open file '',A,
     +'' to record restart data.'')'/
C --- err# 26
      DATA ERRFMT(26) /'(1X,
     +''Cannot write data to run record file.'')'/
C --- err# 27
      DATA ERRFMT(27) /'(1X,''Cannot open file '',A,
     +'' to recommence PEST run record.'')'/
C --- err# 28
      DATA ERRFMT(28) /'(1X,''Cannot restart: PEST was not '',
     +''instructed to record restart file on previous run.'')'/
C --- err# 29
      DATA ERRFMT(29) /'(1X,''Cannot restart: previous PEST run was '',
     +''not halted prematurely.'')'/
C --- err# 30
      DATA ERRFMT(30) /'(1X,''Error writing parameter `'',
     +A,''` to model input file(s): internal error type 1'')'/
C --- err# 31
      DATA ERRFMT(31) /'(1X,''Error writing parameter `'',
     +A,''` to model input file(s): internal error type 2'')'/
C --- err# 32
      DATA ERRFMT(32) /'(1X,''Error writing parameter `'',
     +A,''` to model input file(s): internal error type 3'')'/
C --- err# 33
      DATA ERRFMT(33) /'(1X,''Unbalanced parameter delimiter at '',
     +''line '',A,'' of template file '',A,''.'')'/
C --- err# 34
      DATA ERRFMT(34) /'(1X,''Error writing parameter `'',
     +A,''` to model input file(s): exponent too large or '',
     +''small for single precision protocol.'')'/
C --- err# 35
      DATA ERRFMT(35) /'(1X,''Error writing parameter `'',
     +A,''` to model input file(s): exponent too large or '',
     +''small for double precision protocol.'')'/
C --- err# 36
      DATA ERRFMT(36) /'(1X,''Error writing parameter `'',
     +A,''` to model input file(s). Template field width too '',
     +''small to represent current value; Number too large '',
     +''to fit, or too small to be represented with any precision.'')'/
C --- err# 37
      DATA ERRFMT(37) /'(1X,''Cannot open model input template file '',
     +A,''.'')'/
C --- err# 38
      DATA ERRFMT(38) /'(1X,''Cannot open model input file '',
     +A,'' to update parameter values.'')'/
C --- err# 39
      DATA ERRFMT(39) /'(1X,
     +''Parameter space less than 3 characters wide:'',
     +'' line '',A,'' of template file '',A,''.'')'/
C --- err# 40
      DATA ERRFMT(40) /'(1X,''Third line of file '',
     +A,'' must read `restart` or `norestart`.'')'/
C --- err# 41
      DATA ERRFMT(41) /'(1X,
     +''Cannot swap PESTSW out of memory to run model.'')'/
C --- err# 42
      DATA ERRFMT(42) /'1X,''No entry for DERIVTYP on third line of '',
     +''file '',A,''.'')'/
C --- err# 43
      DATA ERRFMT(43) /'(1X,''Unrecognised parameter precision '',
     +''type (`'',A,''`): line '',A,'' of file '',A,''.'')'/
C --- err# 44
      DATA ERRFMT(44) /'(1X,''Unrecognised decimal point indic'',
     +''ator (`'',A,''`): line '',A,'' of file '',A,''.'')'/
C --- err# 45
      DATA ERRFMT(45) /'(1X,''Observation `'',
     +A,''` not referenced in any instruction file.'')'/
C --- err# 46
      DATA ERRFMT(46) /'(1X,
     +''Parameter cannot be tied to fixed or tied parameter:'',
     +'' line '',A,'' of file '',A,''.'')'/
C --- err# 47
      DATA ERRFMT(47) /'(1X,''Blank parameter space: line '',
     +A,'' of template file '',A,''.'')'/
C --- err# 48
      DATA ERRFMT(48) /'(1X,''Parameter `'',A,''` is not a tied '',
     +''parameter: line '',A,'' of file '',A,''.'')'/
C --- err# 50
      DATA ERRFMT(49) /'(1X,''Parameter `'',
     +A,''` not cited in PEST control file: line '',
     +A,'' of template file '',A,''.'')'/
C --- err# 52
      DATA ERRFMT(50) /'(1X,''Unable to write model input file '',
     +A,'': disk full?'')'/
C --- err# 64
      DATA ERRFMT(51) /'(1X,
     +''Cannot interpret instruction for reading model output file.'',
     +'' Instruction line follows -'')'/
C --- err# 65
      DATA ERRFMT(52) /'(1X,''Cannot open model output file '',
     +A,''.'')'/
C --- err# 66
      DATA ERRFMT(53) /'(1X,''Unexpected end to model output file '',
     +A,''. Instruction line follows -'')'/
C --- err# 67
      DATA ERRFMT(54) /'(1X,''Unable to find secondary marker:'',
     +'' line '',A,'' of model output file '',A,''.'',
     +'' Instruction line follows -'')'/
C --- err# 68
      DATA ERRFMT(55) /'(1X,
     +''Missing marker delimiter. Instruction line follows -'')'/
C --- err# 69
      DATA ERRFMT(56) /'(1X,''Unable to find requested whitespace or'',
     +'' whitespace precedes end of line: line '',A,'' of mod'',
     +''el output file '',A,''. Instruction line follows -'')'/
C --- err# 70
      DATA ERRFMT(57) /'(1X,''Tab moves beyond end of line: '',
     +'' line '',A,'' of model output file '',A,''.'',
     +'' Instruction line follows -'')'/
C --- err# 71
      DATA ERRFMT(58) /'(1X,''Varying one or more parameters has '',
     +''no affect on model output. Try changing initial '',
     +''parameter value(s), increasing derivative increment(s),'',
     +'' or holding the insensitive parameter(s) fixed.'')'/
C --- err# 72
      DATA ERRFMT(59) /'(1X,
     +''Continuation character must be first instruction on line.'',
     +'' Instruction line follows -'')'/
C --- err# 73
      DATA ERRFMT(60) /'(1X,''First instruction line in file cannot '',
     +''start with continuation character.'',
     +'' Instruction line follows -'')'/
C --- err# 74
      DATA ERRFMT(61) /'(1X,''Cannot read tab position.'',
     +'' Instruction line follows -'')'/
C --- err# 75
      DATA ERRFMT(62) /'(1X,''Cannot read line advance item.'',
     +'' Instruction line follows -'')'/
C --- err# 78
      DATA ERRFMT(63) /'(1X,''Cannot open instruction file '',
     +A,''.'')'/
C --- err# 81
      DATA ERRFMT(64) /'(1X,''Backwards move to tab position:'',
     +'' line '',A,'' of model output file '',A,''.'',
     +'' Instruction line follows -'')'/
C --- err# 82
      DATA ERRFMT(65) /'(1X,''Error reading observation `'',A,''`:'',
     +'' line '',A,'' of model output file '',A,''.'')'/
C --- err# 84
      DATA ERRFMT(66) /'(1X,''Observation name `'',
     +A,''` from instruction set not cited in PEST control file.'',
     +'' Instruction line follows -'')'/
C --- err# 88
      DATA ERRFMT(67) /'(1X,''Cannot find observation `'',A,''`:'',
     +'' line '',A,'' of model output file '',A,''.'')'/
C --- err# 90
      DATA ERRFMT(68) /'(1X,''Parameter `'',
     +A,''` not referenced in any template file.'')'/
C --- err# 92
      DATA ERRFMT(69) /'(1X,''File '',
     +A,'' does not have correct template file header.'')'/
C --- err# 93
      DATA ERRFMT(70) /'(1X,''File '',A,
     +'' does not have correct instruction file header.'')'/
C --- err# 94
      DATA ERRFMT(71) /'(1X,''Incorrect parameter name or improper '',
     +''syntax in prior information: line '',A,
     +'' of file '',A,''.'')'/
C --- err# 95
      DATA ERRFMT(72) /'(1X,
     +''Parameter cited in prior information is fixed or tied:'',
     +'' parameter `'',A,''` on line '',A,'' of file '',A,''.'')'/
C --- err# 96
      DATA ERRFMT(73) /'(1X,''Log-transformed parameter referenced '',
     +''as untransformed in prior information: parameter `'',
     +A,''` on line '',A,'' of file '',A,''.'')'/
C --- err# 97
      DATA ERRFMT(74) /'(1X,''Untransformed parameter referenced as '',
     +''log-transformed in prior information: parameter `'',
     +A,''` on line '',A,'' of file '',A,''.'')'/
C --- err# 98
      DATA ERRFMT(75) /'(1X,''Error reading prior information: line '',
     +A,'' of file '',A,''.'')'/
C --- err# 101
      DATA ERRFMT(76) /'(1X,''Cannot calculate derivative for '',
     +''parameter `'',A,''`: increment calculated as frac'',
     +''tion of maximum parameter in group, which is zero.'',
     +'' No increment lower limit provided.'')'/
C --- err# 102
      DATA ERRFMT(77) /'(1X,
     +''Cannot calculate derivative for parameter `'',A,''`:'',
     +'' log-transformed parameter is zero or negative.'')'/
C --- err# 103
      DATA ERRFMT(78) /'(1X,''Cannot calculate derivative for '',
     +''parameter `'',A,''`: parameter zero so increment '',
     +''(calculated as fraction of parameter) is zero.'',
     +'' No increment lower limit provided.'')'/
C --- err# 105
      DATA ERRFMT(79) /'(1X,
     +''Cannot calculate derivative for parameter `'',A,''`:'',
     +'' parameter increment zero to precision allowed by parameter '',
     +''template field width.'')'/
C --- err# 106
      DATA ERRFMT(80) /'(1X,
     +''Internal error in calculating derivative of parameter `'',
     +A,''`. Check all parameter input data with PESTCHEK.'')'/
C --- err# 107
      DATA ERRFMT(81) /'(1X,
     +''Cannot calculate derivative for parameter `'',A,''`:'',
     +'' increase increment because of limited precision allowed by '',
     +''parameter field width on model input file.'')'/
C --- err# 108
      DATA ERRFMT(82) /'(1X,''Error writing parameter `'',
     +A,''` lower or upper bound: internal error type 1'')'/
C --- err# 109
      DATA ERRFMT(83) /'(1X,''Error writing parameter `'',
     +A,''` lower or upper bound: internal error type 2'')'/
C --- err# 110
      DATA ERRFMT(84) /'(1X,''Error writing parameter `'',
     +A,''` lower or upper bound: internal error type 3'')'/
C --- err# 112
      DATA ERRFMT(85) /'(1X,''Error writing parameter `'',
     +A,''` lower or upper bound:'',
     +'' exponent too large or small for single precision '',
     +''protocol.'')'/
C --- err# 113
      DATA ERRFMT(86) /'(1X,''Error writing parameter `'',
     +A,''` lower or upper bound:'',
     +'' exponent too large or small for double precision '',
     +''protocol.'')'/
C --- err# 114
      DATA ERRFMT(87) /'(1X,''Error writing parameter `'',A,''` low'',
     + ''er or upper bound. Template field width too small to'',
     +'' represent value; number too large to fit, or too '',
     +''small to be represented with any precision.'')'/
C --- err# 115
      DATA ERRFMT(88) /'(1X,''Error re-reading PEST control file.'')'/
C --- err# 116
      DATA ERRFMT(89) /'(1X,''Cannot write to run manager record '',
     +''file '',A,''.'')'/
C --- err# 117
      DATA ERRFMT(90) /'(1X,''Cannot open run manager file '',A,
     +''.'')'/
C --- err# 118
      DATA ERRFMT(91) /'(1X,''Cannot communicate with file '',A,
     +''.'')'/
C --- err# 123
      DATA ERRFMT(92) /'(1X,''No agents are alive. '',
     +''Start at least one agent then restart PEST.'')'/
C --- err# 124
      DATA ERRFMT(93) /'(1X,''Cannot close file '',A,''.'')'/
C --- err# 125
      DATA ERRFMT(94) /'(1X,''Cannot delete file '',A,''.'')'/
C --- err# 126
      DATA ERRFMT(95) /'(1X,''Improper switch - `'',A,''`.'')'/
C --- err# 127
      DATA ERRFMT(96) /'(1X,''Jacobian matrix stored in file '',A,
     +'' is not current.'')'/
C --- err# 128
      DATA ERRFMT(97) /'(1X,''Cannot write data to sensitivity '',
     +''record file.'')'/
C --- err# 129
      DATA ERRFMT(98) /'(1X,''Unable to read Jacobian matrix '',
     +''from file '',A,''.'')'/
C --- err# 130
      DATA ERRFMT(99) /'(1X,''Cannot proceed: all parameters '',
     +''either fixed, frozen or held.'')'/
C --- err# 131
      DATA ERRFMT(100) /'(1X,''Cannot open file '',A,
     +'' to read Jacobian matrix.'')'/
C --- err# 132
      DATA ERRFMT(101) /'(1X,''Unrecognized observation group (`'',
     +A,''`): line '',A,'' of file '',A,''.'')'/
C --- err# 133
      DATA ERRFMT(102) /'(1X,''Cannot write data to residuals '',
     +''results file.'')'/
C --- err# 134
      DATA ERRFMT(103) /'(1X,''Error in predictive analyser dataset'',
     +'' - check with PESTCHEK.'')'/
C --- err# 135
      DATA ERRFMT(104) /'(1X,''Observation group `predict` not '',
     +''found in `* observation groups` section of PEST'',
     +'' control file '',A,''.'')'/
C --- err# 136
      DATA ERRFMT(105) /'(1X,''No observation belongs to group '',
     +''`predict` in `* observation data` section of PEST '',
     +''control file '',A,''.'')'/
C --- err# 137
      DATA ERRFMT(106) /'(1X,''Only one observation can belong to '',
     +''group `predict` in `* observation data` section of '',
     +''PEST control file '',A,''.'')'/
C --- err# 138
      DATA ERRFMT(107) /'(1X,''Cannot restart: previous PEST run was '',
     +''in predictive analysis mode.'')'/
C --- err# 139
      DATA ERRFMT(108) /'(1X,''Cannot restart: previous PEST run was '',
     +''in parameter estimation mode.'')'/
C --- err# 140
      DATA ERRFMT(109) /'(1X,''PESTMODE must be `estimation`, '',
     +''`prediction` or `regularisation` '',
     +'' on line 3 of file '',A)'/
C --- err# 141
      DATA ERRFMT(110) /'(1X,''MODPRED must be `together` or '',
     +''`separate` on line 3 of file '',A)'/
C --- err# 142
      DATA ERRFMT(111) /'(1X,''DERIVMOD must be `supplied` or '',
     +''`difference` on line 3 of file '',A)'/
C --- err# 143
      DATA ERRFMT(112) /'(1X,''DERIVPRED must be `supplied` or '',
     +''`difference` on line 3 of file '',A)'/
C --- err# 144
      DATA ERRFMT(113) /'(1X,''Cannot write data to observation '',
     +''sensitivity file.'')'/
C --- err# 145
      DATA ERRFMT(114) /'(1X,''A regularisation group was not found '',
     +''in `* observation groups` section of PEST control '',
     +''file '',A,''.'')'/
C --- err# 146
      DATA ERRFMT(115) /'(1X,''No members of any regularisation '',
     +''group found in PEST control file '',A,''.'')'/
C --- err# 147
      DATA ERRFMT(116) /'(1X,''All observations belonging to '',
     +''regularisation groups have weights of zero in PEST '',
     +''control file '',A,''.'')'/
C --- err# 148
      DATA ERRFMT(117) /'(1X,''Cannot restart: previous PEST run '',
     +''was in regularisation mode.'')'/
C --- err# 149
      DATA ERRFMT(118) /'(1X,''Cannot restart: previous PEST run '',
     +''was not in regularisation mode.'')'/
C --- err# 150
      DATA ERRFMT(119) /'(1X,''Cannot find `* model command line` '',
     +''section of PEST control file '',A,''.'')'/
C --- err# 151
      DATA ERRFMT(120) /'(1X,''Error reading line '',A,'' of file '',
     +A,'': DERCOM value out of bounds.'')'/
C --- err# 152
      DATA ERRFMT(121) /'(1X,''Cannot open external derivatives '',
     +''file '',A,''. Did derivatives model run correctly?'')'/
C --- err# 153
      DATA ERRFMT(122) /'(1X,''Error reading external derivatives '',
     +''file '',A,''.'')'/
C --- err# 154
      DATA ERRFMT(123) /'(1X,''Unexpected end to derivatives '',
     +''file '',A,''.'')'/
C --- err# 155
      DATA ERRFMT(124) /'(1X,''Derivative for observation `'',
     +A,''` with respect to parameter `'',A,''` expected '',
     +'' in derivatives file '',A,''.'')'/
C --- err# 156
      DATA ERRFMT(125) /'(1X,''If a derivative is supplied for a '',
     +''parent parameter it must also be supplied for a '',
     +''tied parameter in file '',A,''.'')'/
C --- err# 157
      DATA ERRFMT(126) /'(1X,''Error reading line '',A,'' of file '',
     +A,'': `* derivatives command line` expected.'')'/
C --- err# 158
      DATA ERRFMT(127) /'(1X,''Parallel PEST cannot be used if '',
     +''there are multiple model command lines.'')'/
C --- err# 159
      DATA ERRFMT(128) /'(1X,''Parallel PEST cannot be used if '',
     +''derivatives are supplied externally.'')'/
C --- err# 160
      DATA ERRFMT(129) /'(1X,''Error reading line '',A,'' of file '',
     +A,'': unexpected `* model input/output` section.'')'/
C --- err# 161
      DATA ERRFMT(130) /'(1X,''Cannot open PEST message file '',A,
     +'' for output.'')'/
C --- err# 162
      DATA ERRFMT(131) /'(1X,''Cannot write to PEST message file '',
     +''pest.pmm.'')'/
C --- err# 163
      DATA ERRFMT(132) /'(1X,''Prediction must be final '',
     +''observation when PEST is run in predictive '',
     +'' analysis mode and JACFILE is set to 1.'')'/
C --- err# 164
      DATA ERRFMT(133) /'(1X,''Incorrect NPAR or NOBS value on '',
     +''first line of derivatives file '',A,''.'')'/
C --- err# 165
      DATA ERRFMT(134) /'(1X,''Cannot open covariance matrix '',
     +''file '',A,''.'')'/
C --- err# 166
      DATA ERRFMT(135) /'(1X,''Error encountered in reading '',
     +''covariance matrix file '',A,''.'')'/
C --- err# 167
      DATA ERRFMT(136) /'(1X,''Unexpected end encountered to '',
     +''covariance matrix file '',A,''.'')'/
C --- err# 168
      DATA ERRFMT(137) /'(1X,''File '',A,'' contains illegal '',
     +''covariance matrix.'')'/
C --- err# 169
      DATA ERRFMT(138) /'(1X,''Cannot calc eigenvalues of '',
     +''covariance matrix supplied for obs grp "'',A,''".'')'/
C --- err# 170
      DATA ERRFMT(139) /'(1X,''If a covariance matrix is supplied'',
     +'', then an observation group can pertain to '',
     +''observations or prior information, but not both.'')'/
C --- err# 171
      DATA ERRFMT(140) /'(1X,''A covariance matrix must not be '',
     +''supplied for observation group "predict" when '',
     +''PEST is run in predictive analysis mode.'')'/
C --- err# 172
      DATA ERRFMT(141) /'(1X,''Both PHIMLIM and PHIMACCEPT '',
     +''must be positive in regularisation section of PEST '',
     +''control file.'')'/
C --- err# 173
      DATA ERRFMT(142) /'(1X,''FRACPHIM must be less than '',
     +''1.0 in regularisation section of PEST '',
     +''control file.'')'/
C --- err# 174
      DATA ERRFMT(143) /'(1X,''PEST control file has an '',
     +''"automatic user intervention" section but'',
     +'' no value has been supplied for DOAUI variable in '',
     +''"control data" section.'')'/
C --- err# 175
      DATA ERRFMT(144) /'(1X,''DOAUI variable has '',
     +''been set to "aui" in PEST control file. However'',
     +'' automatic user intervention can only be done '',
     +''in "parameter estimation" mode.'')'/
C --- err# 176
      DATA ERRFMT(145) /'(1X,''IREGADJ variable '',
     +''must be 0, 1 or 2 in regularisation section of PEST '',
     +'' control file.'')'/
C --- err# 177
      DATA ERRFMT(146) /'(1X,''Memory saver cannot be '',
     +''used unless PEST compiled with ALLOC switch.'')'/
C --- err# 178
      DATA ERRFMT(147) /'(1X,''CG solver cannot be '',
     +''used unless memory conservation is operative.'')'/
C --- err# 179
      DATA ERRFMT(148) /'(1X,''Cannot write data to solver '',
     +''record file.'')'/
C --- err# 180
      DATA ERRFMT(149) /'(1X,''If CG solver is used, values must '',
     +''be supplied for CGRTOL and CGITNLIM.'')'/
C --- err# 181
      DATA ERRFMT(150) /'(1X,''MAXCOMPDIM must be set larger in '',
     +''PEST control file for greater data storage capacity.'',
     +'' Restart with /s or /d switch if appropriate.'')'/
C --- err# 182
      DATA ERRFMT(151) /'(1X,''Jacobian matrix compression cannot '',
     +''be activated if any observation covariance '',
     +'' matrices are supplied.'')'/
C --- err# 183
      DATA ERRFMT(152) /'(1X,''Jacobian matrix compression setting '',
     +''is different in this PEST run from that '',
     +'' in previous PEST run.'')'/
C --- err# 184
      DATA ERRFMT(153) /'(1X,''Data error on first line of '',
     +''derivatives file '',A,''.'')'/
C --- err# 185
      DATA ERRFMT(154) /'(1X,''"/s" switch can only be used '',
     +''for restart with Parallel PEST.'')'/
C --- err# 186
      DATA ERRFMT(155) /'(1X,''An "iw_" parameter must not be '',
     +''cited in a template file.'')'/
C --- err# 187
      DATA ERRFMT(156) /'(1X,''Cannot open file '',A,
     +'' to write resolution data.'')'/
C --- err# 188
      DATA ERRFMT(157) /'(1X,''There are no active agents left.'')'/

