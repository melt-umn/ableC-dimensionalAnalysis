grammar edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:src:concretesyntax:units;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction as abs;


-- Some library utilities and the dimensionalAnalysis abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:src:abstractsyntax ;

marking terminal Units_t 'units' lexer classes {Ckeyword};

terminal Meter_t 'm';
terminal Kilogram_t 'kg';
terminal Second_t 's';
terminal Ampere_t 'A';
terminal Kelvin_t 'K';
terminal Mole_t 'mol';
terminal Candela_t 'cd';

concrete production unitsTypeQualifier_c
top::TypeQualifier_c ::= 'units' p::UnitsParameter_c
{
  top.typeQualifiers = [p.ast];
  top.mutateTypeSpecifiers = [];
}

closed nonterminal UnitsParameter_c with location, ast<abs:Qualifier>;
concrete production unitsParameter_c
top::UnitsParameter_c ::= '(' units::Units_c ')'
{
  top.ast = unitsQualifier(units.ast);
}

closed nonterminal Units_c with location, ast<Units>;
concrete productions top::Units_c
|  us::Units_c '*' u::UnitAndPower_c
  {
    top.ast = consDimUnits(u.ast, us.ast);
  }
| us::Units_c '/' u::UnitAndPower_c
  {
    top.ast = consInvertDimUnits(u.ast, us.ast);
  }
| u::UnitAndPower_c
  {
    top.ast = consDimUnits(u.ast, nilDimUnits());
  }

closed nonterminal UnitAndPower_c with location, ast<Pair<DimUnit Integer>>;
concrete productions top::UnitAndPower_c
| u::Unit_c power::Power_c
  {
    top.ast = pair(u.ast, power.ast);
  }

closed nonterminal Power_c with location, ast<Integer>;
concrete productions top::Power_c
| '^' p::DecConstant_t
  {
    top.ast = toInt(p.lexeme);
  }
| '^' '-' p::DecConstant_t
  {
    top.ast = 0 - toInt(p.lexeme);
  }
|
  {
    top.ast = 1;
  }

closed nonterminal Unit_c with location, ast<DimUnit>;
concrete productions top::Unit_c
| unit::Meter_t
  {
    top.ast = meterUnit();
  }
| unit::Kilogram_t
  {
    top.ast = kilogramUnit();
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


