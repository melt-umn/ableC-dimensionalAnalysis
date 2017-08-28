grammar edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:concretesyntax:convertUnits;

-- Import host language components
imports edu:umn:cs:melt:ableC:concretesyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax as abs;

-- Some library utilities and the dimensionalAnalysis abstract syntax
imports silver:langutil;
imports edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:abstractsyntax;
imports edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:concretesyntax:units;

marking terminal ConvertUnits_t 'convert_units' lexer classes {Ckeyword};

concrete production convertUnitsExpr_c
top::AssignExpr_c ::= 'convert_units' '<' units::UnitsTerm_c '>' '(' e::Expr_c ')'
{
  top.ast = convertUnitsExpr(units.ast, e.ast, location=top.location);
}

