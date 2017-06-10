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
  top.typeQualifiers = p.ast;
  top.mutateTypeSpecifiers = [];
}

closed nonterminal UnitsParameter_c with location, ast<abs:Qualifiers>;
concrete production unitsParameter_c
top::UnitsParameter_c ::= '(' units::UnitsTerm_c ')'
{
  top.ast = abs:foldQualifier([unitsQualifier(units.ast.normalUnits, location=top.location)]);
}

closed nonterminal UnitsTerm_c with location, ast<Units>;
concrete productions top::UnitsTerm_c
| us::UnitsTerm_c '*' u::UnitsExp_c
  {
    top.ast = mulDimUnits(us.ast, u.ast);
  }
| us::UnitsTerm_c '/' u::UnitsExp_c
  {
    top.ast = mulDimUnits(us.ast, expDimUnits(u.ast, -1));
  }
| u::UnitsExp_c
  {
    top.ast = u.ast;
  }

closed nonterminal UnitsExp_c with location, ast<Units>;
concrete productions top::UnitsExp_c
| u::UnitFactor_c '^' power::Power_c
  {
    top.ast = expDimUnits(u.ast, power.ast);
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

closed nonterminal UnitFactor_c with location, ast<Units>;
concrete productions top::UnitFactor_c
| '(' us::UnitsTerm_c ')'
  {
    top.ast = us.ast;
  }
| unit::Meter_t
  {
    top.ast = dimUnit(meterUnit());
  }
| unit::Kilogram_t
  {
    top.ast = dimUnit(kilogramUnit());
  }
| unit::Second_t
  {
    top.ast = dimUnit(secondUnit());
  }
| unit::Ampere_t
  {
    top.ast = dimUnit(ampereUnit());
  }
| unit::Kelvin_t
  {
    top.ast = dimUnit(kelvinUnit());
  }
| unit::Mole_t
  {
    top.ast = dimUnit(moleUnit());
  }
| unit::Candela_t
  {
    top.ast = dimUnit(candelaUnit());
  }


