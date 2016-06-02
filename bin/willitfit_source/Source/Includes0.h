#define pi 3.14159265
#define EstimatedNumberOfStepsInLikelihoodProfile 10
#define MaxNumberOfConstraints 50
#define MaxSizeOfCardfile 1024

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>
#include <time.h>
#include <string.h>

#include "PATH/Auxillary/Parallelisation.h"

// Include supporting headers
#include "PATH/Auxillary/Structs.h"
#include "PATH/Auxillary/Allocation.h"
#include "PATH/Auxillary/AuxillaryFunctions.h"
#include "PATH/Auxillary/IncludeUserDefined.h"

// Include mathematical headers
#include "PATH/Formfactors/FormfactorDebye.h"
#include "PATH/Formfactors/FormfactorHalfBelt.h"
#include "PATH/Formfactors/FormfactorSphere.h"
#include "PATH/Formfactors/FormfactorCylinder.h"
#include "PATH/Formfactors/FormfactorCylinderDisplaced.h"
#include "PATH/Formfactors/FormfactorHammouda.h"
#include "PATH/Formfactors/FormfactorEllipticCylinderWithEllipsoidalEndcaps.h"
#include "PATH/Formfactors/FormfactorRodWithoutEndcaps.h"
