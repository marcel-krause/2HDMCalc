double precision function HHtoDDBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MD2*(-4.D0*MD2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)

 HHtoDDBarTree = totalAmplitude
end function HHtoDDBarTree