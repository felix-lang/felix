set ERROR=0
cd tests
C:/Python34/python.exe run_tests.py
cd ../examples
:: for /D %d in (*) do C:/Python34/Scripts/fbuild.py || set ERROR=1
cd c
C:/Python34/python.exe ../../fbuild-light || set ERROR=1
cd ..
exit %ERROR%
