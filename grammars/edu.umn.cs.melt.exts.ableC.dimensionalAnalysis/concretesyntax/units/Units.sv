grammar edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:concretesyntax:units;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax:host as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the dimensionalAnalysis abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:abstractsyntax;

marking terminal Units_t 'units' lexer classes {Keyword, Global};

terminal Meter_t 'm';
--terminal Kilogram_t 'kg';
terminal Gram_t 'g'; -- just use gram as the base unit
terminal Second_t 's';
terminal Ampere_t 'A';
terminal Kelvin_t 'K';
terminal Mole_t 'mol';
terminal Candela_t 'cd';

terminal Yotta_t 'Y';
terminal Zetta_t 'Z';
terminal Exa_t 'E';
terminal Peta_t 'P';
terminal Tera_t 'T';
terminal Giga_t 'G';
terminal Mega_t 'M';
terminal Kilo_t 'k';
terminal Hecto_t 'h';
terminal Deka_t 'da';
terminal Deci_t 'd';
terminal Centi_t 'c';
--terminal Milli_t 'm';
terminal Micro_t 'u';
terminal Nano_t 'n';
terminal Pico_t 'p';
terminal Femto_t 'f';
terminal Atto_t 'a';
terminal Zepto_t 'z';
terminal Yocto_t 'y';

concrete production unitsTypeQualifier_c
top::TypeQualifier_c ::= 'units' p::UnitsParameter_c
{
  top.typeQualifiers = p.ast;
  top.mutateTypeSpecifiers = [];
}

closed nonterminal UnitsParameter_c with location, ast<abs:Qualifiers>;
concrete production unitsParameter_c
top::UnitsParameter_c ::= '(' units::UnitsTerm_c ')'
{
  top.ast = abs:foldQualifier([unitsQualifier(units.ast.normalUnits, location=top.location)]);
}

closed nonterminal UnitsTerm_c with location, ast<DerivedUnits>;
concrete productions top::UnitsTerm_c
| us::UnitsTerm_c '*' u::UnitsExp_c
  {
    top.ast = mulUnits(us.ast, u.ast);
  }
| us::UnitsTerm_c '/' u::UnitsExp_c
  {
    top.ast = mulUnits(us.ast, expUnits(u.ast, -1));
  }
| u::UnitsExp_c
  {
    top.ast = u.ast;
  }

closed nonterminal UnitsExp_c with location, ast<DerivedUnits>;
concrete productions top::UnitsExp_c
| u::UnitFactor_c '^' power::Power_c
  {
    top.ast = expUnits(u.ast, power.ast);
  }
| u::UnitFactor_c
  {
    top.ast = u.ast;
  }

closed nonterminal Power_c with location, ast<Integer>;
concrete productions top::Power_c
| p::DecConstant_t
  {
    top.ast = toInt(p.lexeme);
  }
| '-' p::DecConstant_t
  {
    top.ast = 0 - toInt(p.lexeme);
  }

closed nonterminal UnitFactor_c with location, ast<DerivedUnits>;
concrete productions top::UnitFactor_c
| '(' us::UnitsTerm_c ')'
  {
    top.ast = us.ast;
  }
| pre::UnitPrefix_c u::BaseUnit_c
  {
    top.ast = scaledUnit(u.ast, pre.ast);
  }
| u::BaseUnit_c
  {
    top.ast = scaledUnit(u.ast, sciExponent(0));
  }

closed nonterminal UnitPrefix_c with location, ast<ConversionFactor>;
concrete productions top::UnitPrefix_c
| pre::Yotta_t
  {
    top.ast = sciExponent(24);
  }
| pre::Zetta_t
  {
    top.ast = sciExponent(21);
  }
| pre::Exa_t
  {
    top.ast = sciExponent(18);
  }
| pre::Peta_t
  {
    top.ast = sciExponent(15);
  }
| pre::Tera_t
  {
    top.ast = sciExponent(12);
  }
| pre::Giga_t
  {
    top.ast = sciExponent(9);
  }
| pre::Mega_t
  {
    top.ast = sciExponent(6);
  }
| pre::Kilo_t
  {
    top.ast = sciExponent(3);
  }
| pre::Hecto_t
  {
    top.ast = sciExponent(2);
  }
| pre::Deka_t
  {
    top.ast = sciExponent(1);
  }
| pre::Deci_t
  {
    top.ast = sciExponent(-1);
  }
| pre::Centi_t
  {
    top.ast = sciExponent(-2);
  }
--| pre::Milli_t
| 'm'
  {
    top.ast = sciExponent(-3);
  }
| pre::Micro_t
  {
    top.ast = sciExponent(-6);
  }
| pre::Nano_t
  {
    top.ast = sciExponent(-9);
  }
| pre::Pico_t
  {
    top.ast = sciExponent(-12);
  }
| pre::Femto_t
  {
    top.ast = sciExponent(-15);
  }
| pre::Atto_t
  {
    top.ast = sciExponent(-18);
  }
| pre::Zepto_t
  {
    top.ast = sciExponent(-21);
  }
| pre::Yocto_t
  {
    top.ast = sciExponent(-24);
  }

closed nonterminal BaseUnit_c with location, ast<BaseUnit>;
concrete productions top::BaseUnit_c
| unit::Meter_t
  {
    top.ast = meterUnit();
  }
--| unit::Kilogram_t
--  {
--    top.ast = kilogramUnit();
--  }
| unit::Gram_t
  {
    top.ast = gramUnit();
  }
| unit::Second_t
  {
    top.ast = secondUnit();
  }
| unit::Ampere_t
  {
    top.ast = ampereUnit();
  }
| unit::Kelvin_t
  {
    top.ast = kelvinUnit();
  }
| unit::Mole_t
  {
    top.ast = moleUnit();
  }
| unit::Candela_t
  {
    top.ast = candelaUnit();
  }


