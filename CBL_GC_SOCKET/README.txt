
 CBL_GC_SOCKET - general purpose socket library

 cob_socket (exported as CBL_GC_SOCKET and for legacy reasons also as
 CBL_OC_SOCKET) is in preparation of being integrated into the GnuCOBOL
 runtime library libcob.

 It can be used for any purpose (even outside of libcob) and is licensed
 under GNU Lesser General Public License version 3+.

 It is perfect to communicate between different machines, running
 different operating-systems and programs written in different
 programming languages.
 COBOL-wise, one common use is the replacement of ACUCOBOL-GTs C$SOCKET.

 Build Hints:
 * for gcc: link against lstdc++; see build.sh
 * for VC: link against AdditionalDependencies="ws2_32.lib";
   see project files under build_windows
