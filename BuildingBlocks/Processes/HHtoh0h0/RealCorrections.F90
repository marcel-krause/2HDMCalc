double precision function HHtoh0h0Real()
 use constants
 use counterterms
 implicit none
#include "looptools.h"

 double precision :: totalAmplitude
 double precision :: p2, p3, E2, E3, I11, I22, I33, I12, I13, I23, m1, m2, m3

 m1 = MHH
 m2 = Mh0
 m3 = Mh0

 totalAmplitude = 0D0

 HHtoh0h0Real = totalAmplitude
end function HHtoh0h0Real
