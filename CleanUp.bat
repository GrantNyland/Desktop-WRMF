@echo off
echo ================================================================================
echo   Cleanup
echo .
echo --------------------------------------------------------------------------------
echo .

del /S /F C:\Users\Public\Documents\Embarcadero\Studio\18.0\Bpl\*.bpl
del /S /F C:\Users\Public\Documents\Embarcadero\Studio\18.0\Bpl\*.rsm
del /S /F C:\Users\Public\Documents\Embarcadero\Studio\18.0\Dcp\*.dcu
del /S /F C:\Users\Public\Documents\Embarcadero\Studio\18.0\Dcp\*.dcp

del /S /F *.~*
del /S /F *.cfg
del /S /F *.dcu
del /S /F *.dcp
del /S /F *.dsk
del /S /F *.dsm
del /S /F *.rsm
del /S /F *.res
del /S /F *.ddp
del /S /F *.dproj.local
del /S /F *.identcache
del /S /F *.stat
del /S /F *.dll
del /S /F *.csv
del /S /F *.txt
del /S /F *.exe
del /S /F *.bpl
del /S /F *.groupproj.local

for /f %%i in ('dir /a:d /s /b __history') do rmdir /s /q %%i
for /f %%i in ('dir /a:d /s /b __recovery') do rmdir /s /q %%i

echo Done.
pause
