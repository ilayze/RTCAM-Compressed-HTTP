# RTCAM-Compressed-HTTP
Implementation and simulation of RTCAM algorithm for compressed HTTP as part of final project in JCE.
The code is written in Scala/Java.

The implementation has a dependency in jnetpcap library: http://jnetpcap.com/ which in windows has a dependency on winpcap(need to install on your computer): http://www.winpcap.org/

For jnetpcap to work I added the jar of jNetPcap version 1.3.0 as a dependency to my project and when running it add: -Djava.library.path="C:\Users\izeidman\Downloads\jnetpcap-1.3.0-1.win64\jnetpcap-1.3.0" to the vm options when running the program.
