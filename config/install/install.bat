@echo off
cls
echo Hitsugaya installation Batch
echo ----------
echo Mappatura di C:\Users\Mattia.IBS\Desktop\Projects\Hitsugaya in H: ...
net use H: C:\Users\Mattia.IBS\Desktop\Projects\Hitsugaya /PERSISTENT:NO
echo ----------
echo Installazione 1 di 1 in corso...
echo Installazione di ciao...
start /wait file.exe
echo ----------
pause
