for %%f in (samples\*.pas) do fpc -Sd %%f
del samples\*.o, samples\*.a
pause
