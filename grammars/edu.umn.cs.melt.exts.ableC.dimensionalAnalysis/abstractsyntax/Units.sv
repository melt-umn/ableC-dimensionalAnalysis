grammar edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:abstractsyntax; 

import edu:umn:cs:melt:ableC:abstractsyntax:host;
import edu:umn:cs:melt:ableC:abstractsyntax:env;
import silver:langutil;
import silver:langutil:pp;
import edu:umn:cs:melt:ableC:abstractsyntax:injectable only runtimeMods, runtimeConversion, injectedQualifiers, rhsRuntimeMod;
import edu:umn:cs:melt:ableC:abstractsyntax:overloadable as ovrld;

global MODULE_NAME :: String = "edu:umn:cs:melt:exts:ableC:dimensionalAnalysis";

-- normalized units, e.g. simplify (seconds*meters*seconds/meters) as (seconds^2)
synthesized attribute normalUnits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> occurs on Qualifier;

aspect default production
top::Qualifier ::=
{
  top.normalUnits = ([], []);
}

abstract production unitsQualifier
top::Qualifier ::= units::Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]>
{
  -- TODO: show conversion factors
  top.pp = text("units(" ++ implode("*", map(showUnit, fst(units))) ++ ")");
  top.mangledName = "units_" ++ implode("_", map(mangleUnit, fst(units)));
  top.qualIsPositive = false;
  top.qualIsNegative = true;
  top.qualAppliesWithinRef = true;
  top.qualCompat = \qualToCompare::Qualifier ->
    unitsCompat(fst(top.normalUnits), fst(qualToCompare.normalUnits)) &&
    null(getConversions(snd(top.normalUnits), snd(qualToCompare.normalUnits)));
--    !convertUnits(snd(top.normalUnits), snd(qualToCompare.normalUnits)).isJust;
--  top.qualConversion = just(\qualToCompare::Qualifier ->
--    convertUnits(snd(top.normalUnits), snd(qualToCompare.normalUnits)));
  top.qualIsHost = false;
  top.normalUnits = normalizeUnits(units);
  top.errors :=
    if   containsMultipleUnits(top.typeToQualify.qualifiers)
    then [err(top.location, "multiple units qualifiers")]
    else [];
}

abstract production convertUnitsExpr
top::Expr ::= convertToUnits::DerivedUnits  e::Expr
{
  -- TODO: fix convertUnitsExpr pp, add pp on DerivedUnits/BaseUnits
  top.pp = ppConcat([ text("convert_units<"), text(">("), e.pp, text(")") ]);
  propagate env, controlStmtContext;

  local eUnits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    collectUnits(e.typerep.qualifiers);

  local compat :: Boolean = unitsCompat(fst(convertToUnits.normalUnits), fst(eUnits));

  top.errors <-
    if   compat
    then []
    else [err(top.location, "units of `convert_units' operands not compatible")];

  top.typerep =
    case convertUnits(snd(convertToUnits.normalUnits), snd(eUnits)) of
      just(conversion) ->
        addQualifiers(
          [unitsQualifier(convertToUnits.normalUnits, location=builtinLoc(MODULE_NAME))],
          dropUnits(forward.typerep)
        )
    | _                -> forward.typerep
    end;

  forwards to
    case convertUnits(snd(convertToUnits.normalUnits), snd(eUnits)) of
      just(conversion) -> conversion(e)
    | _                -> e
    end;
}

nonterminal DerivedUnits with normalUnits;

abstract production mulUnits
top::DerivedUnits ::= us1::DerivedUnits us2::DerivedUnits
{
  top.normalUnits = appendUnits(us1.normalUnits, us2.normalUnits);
}

abstract production expUnits
top::DerivedUnits ::= us::DerivedUnits power::Integer
{
  top.normalUnits = mkExpUnits(us.normalUnits, power);
}

abstract production scaledUnit
top::DerivedUnits ::= u::BaseUnit conversionFactor::ConversionFactor
{
  top.normalUnits = ([(u, 1)], [(conversionFactor, 1)]);
}

-- constant to multiply by to convert to the base unit
nonterminal ConversionFactor with factor;
synthesized attribute factor :: NumericConstant;

-- scientific notation exponent, i.e. x in a*10^x
abstract production sciExponent
top::ConversionFactor ::= e::Integer
{
  top.factor = floatConstant("1E" ++ toString(0 - e), doubleFloatSuffix(), location=builtinLoc(MODULE_NAME));
}

nonterminal BaseUnit with unitEq, ppstr;
synthesized attribute unitEq :: (Boolean ::= BaseUnit);
synthesized attribute ppstr :: String;

abstract production meterUnit
top::BaseUnit ::=
{
  top.unitEq = \unitToCompare :: BaseUnit ->
    case unitToCompare of meterUnit() -> true | _ -> false end;
  top.ppstr = "m";
}

--abstract production kilogramUnit
--top::BaseUnit ::=
--{
--  top.unitEq = \unitToCompare :: BaseUnit ->
--    case unitToCompare of kilogramUnit() -> true | _ -> false end;
--  top.ppstr = "kg";
--}

abstract production gramUnit
top::BaseUnit ::=
{
  top.unitEq = \unitToCompare :: BaseUnit ->
    case unitToCompare of gramUnit() -> true | _ -> false end;
  top.ppstr = "g";
}

abstract production secondUnit
top::BaseUnit ::=
{
  top.unitEq = \unitToCompare :: BaseUnit ->
    case unitToCompare of secondUnit() -> true | _ -> false end;
  top.ppstr = "s";
}

abstract production ampereUnit
top::BaseUnit ::=
{
  top.unitEq = \unitToCompare :: BaseUnit ->
    case unitToCompare of ampereUnit() -> true | _ -> false end;
  top.ppstr = "A";
}

abstract production kelvinUnit
top::BaseUnit ::=
{
  top.unitEq = \unitToCompare :: BaseUnit ->
    case unitToCompare of kelvinUnit() -> true | _ -> false end;
  top.ppstr = "K";
}

abstract production moleUnit
top::BaseUnit ::=
{
  top.unitEq = \unitToCompare :: BaseUnit ->
    case unitToCompare of moleUnit() -> true | _ -> false end;
  top.ppstr = "mol";
}

abstract production candelaUnit
top::BaseUnit ::=
{
  top.unitEq = \unitToCompare :: BaseUnit ->
    case unitToCompare of candelaUnit() -> true | _ -> false end;
  top.ppstr = "cd";
}

aspect production ovrld:addExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  local lunits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    collectUnits(lhs.typerep.qualifiers);
  local runits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    collectUnits(rhs.typerep.qualifiers);

  local compat :: Boolean = unitsCompat(fst(lunits), fst(runits));

  injectedQualifiers <-
    if   compat
    then [unitsQualifier(lunits, location=builtinLoc(MODULE_NAME))]
    else [];

  runtimeMods <-
    if   compat
    then
      case convertUnits(snd(lunits), snd(runits)) of
        just(conversion) -> [rhsRuntimeMod(runtimeConversion(conversion))]
      | _                -> []
      end
    else [];

  lerrors <-
    if   compat
    then []
    else [err(top.location, "units of addition operands not compatible")];
}

aspect production ovrld:subExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  local lunits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    collectUnits(lhs.typerep.qualifiers);
  local runits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    collectUnits(rhs.typerep.qualifiers);

  local compat :: Boolean = unitsCompat(fst(lunits), fst(runits));

  injectedQualifiers <-
    if   compat
    then [unitsQualifier(lunits, location=builtinLoc(MODULE_NAME))]
    else [];

  runtimeMods <-
    if   compat
    then
      case convertUnits(snd(lunits), snd(runits)) of
        just(conversion) -> [rhsRuntimeMod(runtimeConversion(conversion))]
      | _                -> []
      end
    else [];

  lerrors <-
    if   compat
    then []
    else [err(top.location, "units of subtraction operands not compatible")];
}

aspect production ovrld:mulExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  local units :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    normalizeUnits(collectUnits(lhs.typerep.qualifiers ++ rhs.typerep.qualifiers));

  injectedQualifiers <- [unitsQualifier(units, location=builtinLoc(MODULE_NAME))];
}

aspect production ovrld:divExpr
top::Expr ::= lhs::Expr rhs::Expr
{
  local lunits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    collectUnits(lhs.typerep.qualifiers);
  local runits :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    invertUnits(collectUnits(rhs.typerep.qualifiers));

  local units :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    normalizeUnits((fst(lunits) ++ fst(runits), snd(lunits) ++ snd(runits)));

  injectedQualifiers <- [unitsQualifier(units, location=builtinLoc(MODULE_NAME))];
}

function unitsCompat
Boolean ::= xs::[Pair<BaseUnit Integer>]  ys::[Pair<BaseUnit Integer>]
{
  return
    if   null(xs)
    then null(ys)
    else
      case removeUnit(head(xs), ys) of
        just(ysRest) ->
          unitsCompat(tail(xs), ysRest)
      | nothing() -> false
      end;
}

function convertUnits
Maybe<(Expr ::= Expr)> ::= xs::[Pair<ConversionFactor Integer>]
                                     ys::[Pair<ConversionFactor Integer>]
{
  return convertUnitsHelper(getConversions(xs, ys));
}

function convertUnitsHelper
Maybe<(Expr ::= Expr)> ::= conversions::[Pair<ConversionFactor Integer>]
{
  local mRest :: Maybe<(Expr ::= Expr)> = convertUnitsHelper(tail(conversions));

  return
    if   null(conversions)
    then nothing()
    else
      just(
        case mRest of
          just(rest) ->
            \exprToConvert :: Expr ->
              rest(applyConversion(head(conversions))(exprToConvert))
        | nothing()  ->
            \exprToConvert :: Expr ->
              applyConversion(head(conversions))(exprToConvert)
        end
      );
}

function applyConversion
(Expr ::= Expr) ::= conversion::Pair<ConversionFactor Integer>
{
  local power :: Integer = snd(conversion);
  local newPower :: Integer = if power > 0 then power - 1 else power + 1;

  return
    if power == 0
    then \exprToConvert :: Expr -> exprToConvert
    else
      if power > 0
      then
        \exprToConvert :: Expr ->
          ovrld:mulExpr(
            applyConversion((fst(conversion), newPower))(exprToConvert),
            realConstant(
              fst(conversion).factor,
              location=builtinLoc(MODULE_NAME)
            ),
            location=builtinLoc(MODULE_NAME)
          )
      else
        \exprToConvert :: Expr ->
          ovrld:divExpr(
            applyConversion((fst(conversion), newPower))(exprToConvert),
            realConstant(
              fst(conversion).factor,
              location=builtinLoc(MODULE_NAME)
            ),
            location=builtinLoc(MODULE_NAME)
          );
}

function getConversions
[Pair<ConversionFactor Integer>] ::= xs::[Pair<ConversionFactor Integer>]
                                     ys::[Pair<ConversionFactor Integer>]
{
  return
    if   null(xs)
    then ys
    else
      case removeConversionFactor(head(xs), ys) of
        just(ysRest) -> getConversions(tail(xs), ysRest)
      | nothing()    -> error("cannot convert units") -- this shouldn't happen
      end;
}

function removeUnit
Maybe<[Pair<BaseUnit Integer>]> ::= rm::Pair<BaseUnit Integer> xs::[Pair<BaseUnit Integer>]
{
  local x :: Pair<BaseUnit Integer> = head(xs);

  return
    if   null(xs)
    then nothing()
    else
      if fst(rm).unitEq(fst(x))
      then
        -- found match, done
        if   snd(rm) == snd(x)
        then just(tail(xs))
        -- found unit match but not power, subtract and continue
        else removeUnit((fst(rm), snd(rm) - snd(x)), tail(xs))
      else 
        case removeUnit(rm, tail(xs)) of
          just(rest) -> just(cons(x, rest))
        | nothing()  -> nothing()
        end;
}

function removeConversionFactor
Maybe<[Pair<ConversionFactor Integer>]> ::= rm::Pair<ConversionFactor Integer>
                                            xs::[Pair<ConversionFactor Integer>]
{
  local x :: Pair<ConversionFactor Integer> = head(xs);
  local mRest :: Maybe<[Pair<ConversionFactor Integer>]> =
    removeConversionFactor(rm, tail(xs));

  return
    if   null(xs)
    then
      case fst(rm) of
        sciExponent(e1) -> just([(sciExponent(snd(rm) * e1), 1)])
      | _               -> nothing()
      end
    else
      case fst(rm), fst(x) of
        sciExponent(e1), sciExponent(e2) ->
          -- found match, done
          if   snd(rm)*e1 == snd(x)*e2
          then just(tail(xs))
          -- found unit match but not power, subtract then done
          else just(cons((sciExponent(snd(rm)*e1 - snd(x)*e2), 1), tail(xs)))
      | _, _ ->
        case mRest of
          just(rest) -> just(cons(x, rest))
        | nothing()  -> nothing()
        end
      end;
}

function insertBaseUnit
[Pair<BaseUnit Integer>] ::= ins::Pair<BaseUnit Integer>  xs::[Pair<BaseUnit Integer>]
{
  local x :: Pair<BaseUnit Integer> = head(xs);

  return
    if   null(xs)
    then [ins]
    else
      if fst(ins).unitEq(fst(x))
      then
        if   snd(ins) == 0 - snd(x)
        then tail(xs)
        else cons((fst(ins), snd(ins) + snd(x)), tail(xs))
      else
        cons(x, insertBaseUnit(ins, tail(xs)));
}

function insertConversionFactor
[Pair<ConversionFactor Integer>] ::= ins::Pair<ConversionFactor Integer>
                                     xs::[Pair<ConversionFactor Integer>]
{
  local x :: Pair<ConversionFactor Integer> = head(xs);

  return
    if   null(xs)
    then [ins]
    else
      case fst(ins), fst(x) of
        sciExponent(e1), sciExponent(e2) ->
          if   snd(ins)*e1 == 0 - snd(x)*e2
          then tail(xs)
          else cons((sciExponent(snd(ins)*e1 + snd(x)*e2), 1), tail(xs))
      | _, _ -> cons(x, insertConversionFactor(ins, tail(xs)))
      end;
}

function collectUnits
Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> ::= qs::[Qualifier]
{
  local q :: Qualifier = head(qs);
  local rest :: Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> =
    collectUnits(tail(qs));

  return
    if   null(qs)
    then ([], [])
    else
      case q of
        unitsQualifier(_) ->
          appendUnits(q.normalUnits, rest)
      | _ -> rest
      end;
}

function dropUnits
Type ::= ty::Type
{
  return
    addQualifiers(
      filter(
        \q::Qualifier -> case q of unitsQualifier(_) -> false | _ -> true end,
        ty.qualifiers
      ),
      ty.withoutTypeQualifiers
    );
}

function normalizeUnits
Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> ::=
  xs::Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]>
{
  return (normalizeBaseUnits(fst(xs)), normalizeConversionFactors(snd(xs)));
}

function normalizeBaseUnits
[Pair<BaseUnit Integer>] ::= xs::[Pair<BaseUnit Integer>]
{
  return
    if null(xs)
    then []
    else insertBaseUnit(head(xs), normalizeBaseUnits(tail(xs)));
}

function normalizeConversionFactors
[Pair<ConversionFactor Integer>] ::= xs::[Pair<ConversionFactor Integer>]
{
  return
    if null(xs)
    then []
    else insertConversionFactor(head(xs), normalizeConversionFactors(tail(xs)));
}

function appendUnits
Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> ::=
  xs1::Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]>
  xs2::Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]>
{
  return (appendBaseUnits(fst(xs1), fst(xs2)), appendConversionFactors(snd(xs1), snd(xs2)));
}

function appendBaseUnits
[Pair<BaseUnit Integer>] ::= xs1::[Pair<BaseUnit Integer>] xs2::[Pair<BaseUnit Integer>]
{
  return
    if   null(xs1)
    then xs2
    else appendBaseUnits(tail(xs1), insertBaseUnit(head(xs1), xs2));
}

function appendConversionFactors
[Pair<ConversionFactor Integer>] ::= xs1::[Pair<ConversionFactor Integer>]
                                     xs2::[Pair<ConversionFactor Integer>]
{
  return
    if   null(xs1)
    then xs2
    else appendConversionFactors(tail(xs1), insertConversionFactor(head(xs1), xs2));
}

function invertUnits
Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> ::=
  xs::Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]>
{
  return mkExpUnits(xs, -1);
}

function mkExpUnits
Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]> ::=
  xs::Pair<[Pair<BaseUnit Integer>] [Pair<ConversionFactor Integer>]>
  power::Integer
{
  local eu :: (Pair<BaseUnit Integer> ::= Pair<BaseUnit Integer>) =
    \u :: Pair<BaseUnit Integer> -> (fst(u), power * snd(u));
  local ec :: (Pair<ConversionFactor Integer> ::= Pair<ConversionFactor Integer>) =
    \c :: Pair<ConversionFactor Integer> -> (fst(c), power * snd(c));
  return (map(eu, fst(xs)), map(ec, snd(xs)));
}

function showUnit
String ::= u::Pair<BaseUnit Integer>
{
  local power :: Integer = snd(u);
  return
    if   power == 1
    then fst(u).ppstr
    else fst(u).ppstr ++ "^" ++ toString(power);
}

function mangleUnit
String ::= u::Pair<BaseUnit Integer>
{
  local power :: Integer = snd(u);
  return
    if   power == 1
    then fst(u).ppstr
    else fst(u).ppstr ++ "_" ++ toString(power);
}

function containsMultipleUnits
Boolean ::= qs::[Qualifier]
{
  local h :: Qualifier = head(qs);
  local t :: [Qualifier] = tail(qs);
  return
    if   null(qs)
    then false
    else
      case h of
        unitsQualifier(_) -> containsUnits(t)
      | _                 -> containsMultipleUnits(t)
      end;
}

function containsUnits
Boolean ::= qs::[Qualifier]
{
  local h :: Qualifier = head(qs);
  local t :: [Qualifier] = tail(qs);
  return
    if   null(qs)
    then false
    else
      case h of
        unitsQualifier(_) -> true
      | _                 -> containsUnits(t)
      end;
}

