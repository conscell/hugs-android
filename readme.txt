This is a port of HUGS Haskell interpreter to Android.

Build
------

To compile hugs-android, you have to install Android NDK and add the
path to your NDK directory into the PATH variable. For example:

export PATH=${PATH}:~/Development/android-ndk-r9 

Either you can build it manually by executing 'ndk-build' command 
or by executing 'tools/build.sh' script in the 'hugs-android' directory,
which will also create a ready to install archive 'hugs.zip' and the
installation script 'inst.sh'. The default target architecture is
'armeabi'. To build application for the other architecture you have to edit
files 'jni/Application.mk' and 'tools/build.sh' and replace the value
'armeabi' to one of the following: 'armeabi-v7a' 'mips' 'x86'.

Install
--------

This port is intended for use with Jack Palevich's Android Terminal Emulator
which is available here: 

https://play.google.com/store/apps/details?id=jackpal.androidterm

After installing the terminal emulator you need to download the 
archive 'hugs.zip' to your device, decompress it and then copy 
extracted 'hugs' directory to the terminal emulator application home 
directory '/data/data/jackpal.androidterm/app_HOME'.
Since stock android doesn't have a 'cp' command which can copy directories
recursively, you can use the installation script 'inst.sh'. It will
do the job by creating the directory hierarchy with 'mkdir' command 
and copying files with 'cat' command from the download directory
'/sdcard/Download' to the application home directory.  If you have
a different download and application home locations you need to change the
values of variables DLDIR and BASEDIR in the 'inst.sh' script.

Happy coding!
