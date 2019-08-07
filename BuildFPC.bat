copy XDPWCustomDelphi.inc XDPWCustom.inc /Y
rename system.pas _system.pas
fpc -Sd xdpw.pas
rename _system.pas system.pas
del *.o
