double precision function h0toDDBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MD2*(-4.D0*MD2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)

 h0toDDBarTree = totalAmplitude
end function h0toDDBarTree