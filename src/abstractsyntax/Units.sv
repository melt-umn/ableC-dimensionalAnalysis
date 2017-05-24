grammar edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:src:abstractsyntax; 

imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports silver:langutil;
imports silver:langutil:pp;

-- normalized units, e.g. simplify (seconds*meters*seconds/meters) as (seconds^2)
synthesized attribute normalUnits :: [Pair<DimUnit Integer>] occurs on Qualifier;

aspect default production
top::Qualifier ::=
{
  top.normalUnits = [];
}

abstract production unitsQualifier
top::Qualifier ::= units::[Pair<DimUnit Integer>]
{
  top.pp = text("units(" ++ implode("*", map(showUnit, units)) ++ ")");
  top.mangledName = "units_" ++ implode("_", map(mangleUnit, units));
  top.qualIsPositive = false;
  top.qualIsNegative = true;
  top.qualAppliesWithinRef = true;
  top.qualCompat = \qualToCompare::Qualifier ->
    unitsCompat(top.normalUnits, qualToCompare.normalUnits);
  top.qualIsHost = false;
  top.normalUnits = units;
}

nonterminal Units with normalUnits;

abstract production mulDimUnits
top::Units ::= us1::Units us2::Units
{
  top.normalUnits = appendUnits(us1.normalUnits, us2.normalUnits);
}

abstract production expDimUnits
top::Units ::= us::Units power::Integer
{
  top.normalUnits = expUnits(us.normalUnits, power);
}

abstract production dimUnit
top::Units ::= u::DimUnit
{
  top.normalUnits = [pair(u, 1)];
}

nonterminal DimUnit with unitEq, ppstr;
synthesized attribute unitEq :: (Boolean ::= DimUnit);
synthesized attribute ppstr :: String;

abstract production meterUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of meterUnit() -> true | _ -> false end;
  top.ppstr = "m";
}

abstract production kilogramUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of kilogramUnit() -> true | _ -> false end;
  top.ppstr = "kg";
}

abstract production secondUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of secondUnit() -> true | _ -> false end;
  top.ppstr = "s";
}

abstract production ampereUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of ampereUnit() -> true | _ -> false end;
  top.ppstr = "A";
}

abstract production kelvinUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of kelvinUnit() -> true | _ -> false end;
  top.ppstr = "K";
}

abstract production moleUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of moleUnit() -> true | _ -> false end;
  top.ppstr = "mol";
}

abstract production candelaUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of candelaUnit() -> true | _ -> false end;
  top.ppstr = "cd";
}

aspect production addOp
top::NumOp ::=
{
  local lunits :: [Pair<DimUnit Integer>] =
    collectUnits(top.lop.typerep.qualifiers);
  local runits :: [Pair<DimUnit Integer>] =
    collectUnits(top.rop.typerep.qualifiers);

  -- FIXME: exceeding flow type
  top.collectedTypeQualifiers <-
    if   unitsCompat(lunits, runits)
    then [unitsQualifier(lunits)]
    else [];
}

aspect production subOp
top::NumOp ::=
{
  local lunits :: [Pair<DimUnit Integer>] =
    collectUnits(top.lop.typerep.qualifiers);
  local runits :: [Pair<DimUnit Integer>] =
    collectUnits(top.rop.typerep.qualifiers);

  -- FIXME: exceeding flow type
  top.collectedTypeQualifiers <-
    if   unitsCompat(lunits, runits)
    then [unitsQualifier(lunits)]
    else [];
}

aspect production mulOp
top::NumOp ::=
{
  local units :: [Pair<DimUnit Integer>] =
    collectUnits(top.lop.typerep.qualifiers ++ top.rop.typerep.qualifiers);

  -- FIXME: exceeding flow type
  top.collectedTypeQualifiers <- [unitsQualifier(units)];
}

aspect production divOp
top::NumOp ::=
{
  local units :: [Pair<DimUnit Integer>] =
    collectUnits(top.lop.typerep.qualifiers) ++
      invertUnits(collectUnits(top.rop.typerep.qualifiers));

  -- FIXME: exceeding flow type
  top.collectedTypeQualifiers <- [unitsQualifier(units)];
}

function unitsCompat
Boolean ::= xs::[Pair<DimUnit Integer>] ys::[Pair<DimUnit Integer>]
{
  return
    if   null(xs)
    then null(ys)
    else
      case removeUnit(head(xs), ys) of
        just(ysRest) -> unitsCompat(tail(xs), ysRest)
      | nothing() -> false
      end;
}

function removeUnit
Maybe<[Pair<DimUnit Integer>]> ::= rm::Pair<DimUnit Integer> xs::[Pair<DimUnit Integer>]
{
  local x :: Pair<DimUnit Integer> = head(xs);

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
        else removeUnit(pair(fst(rm), snd(rm) - snd(x)), tail(xs))
      else 
        case removeUnit(rm, tail(xs)) of
          just(rest) -> just(cons(x, rest))
        | nothing()  -> nothing()
        end;
}

function insertUnit
[Pair<DimUnit Integer>] ::= ins::Pair<DimUnit Integer>  xs::[Pair<DimUnit Integer>]
{
  local x :: Pair<DimUnit Integer> = head(xs);

  return
    if   null(xs)
    then [ins]
    else
      if fst(ins).unitEq(fst(x))
      then
        if   snd(ins) == 0 - snd(x)
        then tail(xs)
        else cons(pair(fst(ins), snd(ins) + snd(x)), tail(xs))
      else
        cons(x, insertUnit(ins, tail(xs)));
}

function collectUnits
[Pair<DimUnit Integer>] ::= qs::[Qualifier]
{
  local q :: Qualifier = head(qs);
  local rest :: [Pair<DimUnit Integer>] = collectUnits(tail(qs));

  return
    if   null(qs)
    then []
    else
      case q of
        unitsQualifier(_) -> q.normalUnits ++ rest
      | _                 -> rest
      end;
}

function appendUnits
[Pair<DimUnit Integer>] ::= xs1::[Pair<DimUnit Integer>]  xs2::[Pair<DimUnit Integer>]
{
  return
    if   null(xs1)
    then xs2
    else appendUnits(tail(xs1), insertUnit(head(xs1), xs2));
}

function invertUnits
[Pair<DimUnit Integer>] ::= xs::[Pair<DimUnit Integer>]
{
  return expUnits(xs, -1);
}

function expUnits
[Pair<DimUnit Integer>] ::= xs::[Pair<DimUnit Integer>]  power::Integer
{
  local eu :: (Pair<DimUnit Integer> ::= Pair<DimUnit Integer>) =
    \u :: Pair<DimUnit Integer> -> pair(fst(u), power * snd(u));
  return map(eu, xs);
}

function showUnit
String ::= u::Pair<DimUnit Integer>
{
  local power :: Integer = snd(u);
  return
    if   power == 1
    then fst(u).ppstr
    else fst(u).ppstr ++ "^" ++ toString(power);
}

function mangleUnit
String ::= u::Pair<DimUnit Integer>
{
  local power :: Integer = snd(u);
  return
    if   power == 1
    then fst(u).ppstr
    else fst(u).ppstr ++ "_" ++ toString(power);
}

