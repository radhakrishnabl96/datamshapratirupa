#@echo off

# Intermediate files are deleted

rm in.dat
rm out.dat

# First the model is run under calibration conditions

cp in1.dat in.dat > /dev/null
./twoline > /dev/null
cp out.dat out1.dat > /dev/null

# Next the model is run under predictive conditions

cp in2.dat in.dat > /dev/null
./twoline > /dev/null
cp out.dat out2.dat > /dev/null
