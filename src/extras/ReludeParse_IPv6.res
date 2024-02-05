@@uncurried
@@uncurried.swap

open Relude.Globals
module P = ReludeParse_Parser
open P

// TODO: not sure if this parser/show is 100 correct for all the possible the
// abbreviated formats

type t = IPv6(int, int, int, int, int, int, int, int)

let make = (a, b, c, d, e, f, g, h) => IPv6(a, b, c, d, e, f, g, h)

let loopback = IPv6(0, 0, 0, 0, 0, 0, 0, 1)

// TODO: abbreviated forms like 2001::8342
let show: t => string = x =>
  switch x {
  | IPv6(0, 0, 0, 0, 0, 0, 0, 1) => "::1"
  | IPv6(a, b, c, d, e, f, g, h) =>
    list{a, b, c, d, e, f, g, h}
    ->List.map(x => Js.Int.toStringWithRadix(~radix=16, x), _)
    ->(List.String.joinWith(":", _))
  }

let allZeroGroup: P.t<int> = timesMinMax(1, 4, str("0"))->\"<$$>"(_ => 0)

let threeZeroPadGroup = {
  times(3, str("0"))
  ->\"*>"(anyHexDigit)
  ->\"<$$>"(hexDigit => int_of_string("0x" ++ hexDigit))
}

let twoZeroPadGroup = {
  times(2, str("0"))
  ->\"*>"(timesMinMax(1, 2, anyHexDigit))
  ->\"<$$>"(List.String.join)
  ->\"<$$>"(hexDigits => int_of_string("0x" ++ hexDigits))
}

let oneZeroPadGroup = {
  str("0")
  ->\"*>"(timesMinMax(1, 3, anyHexDigit))
  ->\"<$$>"(List.String.join)
  ->\"<$$>"(hexDigits => int_of_string("0x" ++ hexDigits))
}
let nonZeroPaddedGroup: P.t<int> = {
  (anyNonZeroHexDigit, timesMax(3, anyHexDigit)->\"<$$>"(List.String.join))
  ->mapTuple2((first, rest) => first ++ rest, _)
  ->\"<$$>"(hexDigits => int_of_string("0x" ++ hexDigits))
}

let emptyGroup: P.t<int> = str("")->\"<$$>"(_ => 0)

let group: P.t<int> = {
  tries(threeZeroPadGroup)
  ->\"<|>"(tries(twoZeroPadGroup))
  ->\"<|>"(tries(oneZeroPadGroup))
  ->\"<|>"(tries(nonZeroPaddedGroup))
  ->\"<|>"(allZeroGroup)
  ->\"<|>"(emptyGroup)
  ->\"<?>"("Expected IPv6 hex value")
}

let groupsNel: P.t<Nel.t<int>> = sepBy1(str(":"), group)

let groups: P.t<t> = groupsNel->\">>="(x =>
  switch x {
  | NonEmpty(a, list{b, c, d, e, f, g, h}) => pure(make(a, b, c, d, e, f, g, h))

  | NonEmpty(a, list{0, c, d, e, f, g}) => pure(make(a, 0, 0, c, d, e, f, g))
  | NonEmpty(a, list{b, 0, d, e, f, g}) => pure(make(a, b, 0, 0, d, e, f, g))
  | NonEmpty(a, list{b, c, 0, e, f, g}) => pure(make(a, b, c, 0, 0, e, f, g))
  | NonEmpty(a, list{b, c, d, 0, f, g}) => pure(make(a, b, c, d, 0, 0, f, g))
  | NonEmpty(a, list{b, c, d, e, 0, g}) => pure(make(a, b, c, d, e, 0, 0, g))
  | NonEmpty(a, list{b, c, d, e, f, 0}) => pure(make(a, b, c, d, e, f, 0, 0))

  | NonEmpty(a, list{0, c, d, e, f}) => pure(make(a, 0, 0, 0, c, d, e, f))
  | NonEmpty(a, list{b, 0, d, e, f}) => pure(make(a, b, 0, 0, 0, d, e, f))
  | NonEmpty(a, list{b, c, 0, e, f}) => pure(make(a, b, c, 0, 0, 0, e, f))
  | NonEmpty(a, list{b, c, d, 0, f}) => pure(make(a, b, c, d, 0, 0, 0, f))
  | NonEmpty(a, list{b, c, d, e, 0}) => pure(make(a, b, c, d, e, 0, 0, 0))

  | NonEmpty(a, list{0, c, d, e}) => pure(make(a, 0, 0, 0, 0, c, d, e))
  | NonEmpty(a, list{b, 0, d, e}) => pure(make(a, b, 0, 0, 0, 0, d, e))
  | NonEmpty(a, list{b, c, 0, e}) => pure(make(a, b, c, 0, 0, 0, 0, e))
  | NonEmpty(a, list{b, c, d, 0}) => pure(make(a, b, c, d, 0, 0, 0, 0))

  | NonEmpty(a, list{0, c, d}) => pure(make(a, 0, 0, 0, 0, 0, c, d))
  | NonEmpty(a, list{b, 0, d}) => pure(make(a, b, 0, 0, 0, 0, 0, d))
  | NonEmpty(a, list{b, c, 0}) => pure(make(a, b, c, 0, 0, 0, 0, 0))

  | NonEmpty(a, list{0, c}) => pure(make(a, 0, 0, 0, 0, 0, 0, c))
  | NonEmpty(a, list{b, 0}) => pure(make(a, b, 0, 0, 0, 0, 0, 0))

  | NonEmpty(a, list{0}) => pure(make(a, 0, 0, 0, 0, 0, 0, 0))

  | NonEmpty(_) => fail("Failed to parse IPv6 address")
  }
)

let loopbackAbbreviated = str("::1")->\"<$$>"(const(loopback, _))

let parser: P.t<t> = loopbackAbbreviated->\"<|>"(groups)

let parse: string => Belt.Result.t<t, P.ParseError.t> = str => P.runParser(str, parser)

let parseOption: string => option<t> = \">>"(parse, Result.getOk, _)

let unsafeFromString: string => t = str =>
  Result.fold(e => failwith(P.ParseError.show(e)), id, parse(str))

let unsafeFromInts = (a, b, c, d, e, f, g, h) =>
  unsafeFromString(show(make(a, b, c, d, e, f, g, h)))
