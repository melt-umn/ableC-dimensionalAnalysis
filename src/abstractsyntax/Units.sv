grammar edu:umn:cs:melt:exts:ableC:dimensionalAnalysis:src:abstractsyntax; 

imports edu:umn:cs:melt:ableC:abstractsyntax;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports silver:langutil;
imports silver:langutil:pp;

-- normalized units, e.g. simplify (seconds*meters*seconds/meters) as (seconds^2)
synthesized attribute normalUnits :: [Pair<DimUnit Integer>] occurs on Qualifier;

abstract production unitsQualifier
top::Qualifier ::= units::Units
{
  local isPositive :: Boolean = false;
  local appliesWithinRef :: Boolean = true;
  local compat :: (Boolean ::= Qualifier) = \qualToCompare::Qualifier ->
    unitsCompat(top.normalUnits, qualToCompare.normalUnits);
  top.normalUnits = units.normalUnits;
  forwards to pluggableQualifier(isPositive, appliesWithinRef, compat);
}

aspect production constQualifier
top::Qualifier ::=
{
  top.normalUnits = [];
}

aspect production volatileQualifier
top::Qualifier ::=
{
  top.normalUnits = [];
}

aspect production restrictQualifier
top::Qualifier ::=
{
  top.normalUnits = [];
}

aspect production uuRestrictQualifier
top::Qualifier ::=
{
  top.normalUnits = [];
}

aspect production pluggableQualifier
top::Qualifier ::= isPositive::Boolean appliesWithinRef::Boolean
                   compat::(Boolean ::= Qualifier)
{
  top.normalUnits = [];
}

nonterminal Units with normalUnits;

abstract production consDimUnits
top::Units ::= u::Pair<DimUnit Integer>  us::Units
{
  top.normalUnits = insertUnit(u, us.normalUnits);
}

abstract production consInvertDimUnits
top::Units ::= u::Pair<DimUnit Integer>  us::Units
{
  local invertU :: Pair<DimUnit Integer> =
    pair(fst(u), 0 - snd(u));
  top.normalUnits = insertUnit(invertU, us.normalUnits);
}

abstract production nilDimUnits
top::Units ::=
{
  top.normalUnits = [];
}

nonterminal DimUnit with unitEq;
synthesized attribute unitEq :: (Boolean ::= DimUnit);

abstract production meterUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of meterUnit() -> true | _ -> false end;
}

abstract production kilogramUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of kilogramUnit() -> true | _ -> false end;
}

abstract production secondUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of secondUnit() -> true | _ -> false end;
}

abstract production ampereUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of ampereUnit() -> true | _ -> false end;
}

abstract production kelvinUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of kelvinUnit() -> true | _ -> false end;
}

abstract production moleUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of moleUnit() -> true | _ -> false end;
}

abstract production candelaUnit
top::DimUnit ::=
{
  top.unitEq = \unitToCompare :: DimUnit ->
    case unitToCompare of candelaUnit() -> true | _ -> false end;
}

aspect production addOp
top::NumOp ::=
{
  local lunits :: Units =
    listToUnits(collectUnits(getQualifiers(top.lop.typerep)));
  local runits :: Units =
    listToUnits(collectUnits(getQualifiers(top.rop.typerep)));

  local units :: Units =
    listToUnits(collectUnits(getQualifiers(top.lop.typerep)));

  -- FIXME: exceeding flow type
  top.collectedTypeQualifiers <-
    if   unitsCompat(lunits.normalUnits, runits.normalUnits)
    then [unitsQualifier(units)]
    else [];
}

aspect production subOp
top::NumOp ::=
{
  local lunits :: Units =
    listToUnits(collectUnits(getQualifiers(top.lop.typerep)));
  local runits :: Units =
    listToUnits(collectUnits(getQualifiers(top.rop.typerep)));

  local units :: Units =
    listToUnits(collectUnits(getQualifiers(top.lop.typerep)));

  -- FIXME: exceeding flow type
  top.collectedTypeQualifiers <-
    if   unitsCompat(lunits.normalUnits, runits.normalUnits)
    then [unitsQualifier(units)]
    else [];
}

aspect production mulOp
top::NumOp ::=
{
  local units :: Units =
    listToUnits(collectUnits(getQualifiers(top.lop.typerep) ++ getQualifiers(top.rop.typerep)));
  -- FIXME: exceeding flow type
  top.collectedTypeQualifiers <- [unitsQualifier(units)];
}

aspect production divOp
top::NumOp ::=
{
  local units :: Units =
    listToUnits(collectUnits(getQualifiers(top.lop.typerep)) ++
      invertUnits(collectUnits(getQualifiers(top.rop.typerep))));

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
--        unitsQualifier(_) -> appendUnits(q.normalUnits, rest)
        unitsQualifier(_) -> q.normalUnits ++ rest
      | _                 -> rest
      end;
--  return consDimUnits(pair(meterUnit(), 2), nilDimUnits());
}

--function appendUnits
--[Pair<DimUnit Integer>] ::= xs1::[Pair<DimUnit Integer>]  xs2::[Pair<DimUnit Integer>]
--{
--  return
--    if   null(xs1)
--    then xs2
--    else appendUnits(tail(xs1), insertUnit(head(xs1), xs2));
--}

function listToUnits
Units ::= xs::[Pair<DimUnit Integer>]
{
  return
    if   null(xs)
    then nilDimUnits()
    else consDimUnits(head(xs), listToUnits(tail(xs)));
}

function invertUnits
[Pair<DimUnit Integer>] ::= xs::[Pair<DimUnit Integer>]
{
  local x :: Pair<DimUnit Integer> = head(xs);
  local invertX :: Pair<DimUnit Integer> =
    pair(fst(x), 0 - snd(x));

  return
    if   null(xs)
    then []
    else cons(invertX, invertUnits(tail(xs)));
}

