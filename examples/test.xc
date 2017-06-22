#include <stdlib.h>

const units(m) double METER = (units(m) double) 1.;
const units(mm) double MILLIMETER = (units(mm) double) 1.;
const units(kg) double KILOGRAM = (units(kg) double) 1.;
const units(s) double SECOND = (units(s) double) 1.;
const units(A) double AMPERE = (units(A) double) 1.;
const units(K) double KELVIN = (units(K) double) 1.;
const units(mol) double MOLE = (units(mol) double) 1.;
const units(cd) double CANDELA = (units(cd) double) 1.;

int main(void)
{
    units(m) double length = 5. * METER;
    units(mm) double width = 7000. * MILLIMETER;
    units(m) double height = 11. * METER;
    units(kg) double mass = 13. * KILOGRAM;

    /* when adding, units remain the same */
    /* width will be automatically converted from mm to m at runtime */
    units(m) double perim = 2. * (length + width);

    /* when multiplying, units also multiply */
    units(m^2) double area = length * width;

    /* when dividing, units also divide */
    units(kg/m^3) double density = mass / (area * height);

    /* type error, wrong units */
//    units(kg*s) double wrong = mass * length;

    return 0;
}

