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

#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Auxillary/Parallelisation.h"

// Include supporting headers
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Auxillary/Structs.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Auxillary/Allocation.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Auxillary/AuxillaryFunctions.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Auxillary/IncludeUserDefined.h"

// Include mathematical headers
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorDebye.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorHalfBelt.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorSphere.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorCylinder.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorCylinderDisplaced.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorHammouda.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorEllipticCylinderWithEllipsoidalEndcaps.h"
#include "/share/apps/genapp/willitfit/bin/willitfit_source/Source/Formfactors/FormfactorRodWithoutEndcaps.h"
