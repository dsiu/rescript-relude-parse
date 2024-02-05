@@uncurried
@@uncurried.swap

open Relude.Globals
module Parser = ReludeParse_Parser

module AreaCode = {
  type t = AreaCode(int, int, int)

  let parser = {
    open Parser
    (opt(str("(")), filter(v => v != 1, anyDigitAsInt), times2(anyDigitAsInt), opt(str(")")))->(
      mapTuple4((_, a, (b, c), _) => AreaCode(a, b, c), _)
    )
  }
}

module Exchange = {
  type t = Exchange(int, int, int)

  // TODO: I think this should disallow 1 as the first digit as well as the
  // pattern x11
  let parser = {
    open Parser
    times3(anyDigitAsInt)->(map(((a, b, c)) => Exchange(a, b, c), _))
  }
}

module Line = {
  type t = Line(int, int, int, int)

  let parser = {
    open Parser
    times4(anyDigitAsInt)->(map(((a, b, c, d)) => Line(a, b, c, d), _))
  }
}

type t = NanpPhone(AreaCode.t, Exchange.t, Line.t)

let make = (areaCode, exchange, line) => NanpPhone(areaCode, exchange, line)

let toDigits = (NanpPhone(AreaCode(a, b, c), Exchange(d, e, f), Line(g, h, i, j))) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
)

let sep = Parser.anyOfStr(list{"-", "."})

let parser: Parser.t<t> = {
  open Parser

  {
    open Relude.Function
    make->uncurryFn3
  }
  ->\"<$>"(
    ws
    ->\"*>"(opt(str("+")->\"*>"(str("1"))->\"<|>"(str("1"))))
    ->\"*>"(ws)
    ->\"*>"(AreaCode.parser),
  )
  ->\"<*"(opt(sep))
  ->\"<*"(ws)
  ->\"<*>"(Exchange.parser)
  ->\"<*"(opt(sep))
  ->\"<*"(ws)
  ->\"<*>"(Line.parser)
  ->\"<*"(ws)
  ->\"<*"(eof)
}

let parse = str => Parser.runParser(str, parser)

let parseOption = \">>"(parse, Result.getOk, _)

let unsafeFromString = str =>
  parse(str)->(Result.fold(e => failwith(ReludeParse_Parser.ParseError.show(e)), id, _))

type format =
  | Local // 754-3010
  | Domestic // 541-754-3010
  | DomesticShort // 5417543010
  | DomesticParen // (541) 754-3010
  | International // +1-541-754-3010
  | InternationalShort // +15417543010

let show = (~format=Domestic, phone: t): string => {
  let (a, b, c, d, e, f, g, h, i, j) = toDigits(phone)
  switch format {
  | Local =>
    string_of_int(d) ++
    (string_of_int(e) ++
    (string_of_int(f) ++
    ("-" ++
    (string_of_int(g) ++ (string_of_int(h) ++ (string_of_int(i) ++ string_of_int(j)))))))

  | Domestic =>
    string_of_int(a) ++
    (string_of_int(b) ++
    (string_of_int(c) ++
    ("-" ++
    (string_of_int(d) ++
    (string_of_int(e) ++
    (string_of_int(f) ++
    ("-" ++
    (string_of_int(g) ++ (string_of_int(h) ++ (string_of_int(i) ++ string_of_int(j)))))))))))

  | DomesticShort =>
    string_of_int(a) ++
    (string_of_int(b) ++
    (string_of_int(c) ++
    (string_of_int(d) ++
    (string_of_int(e) ++
    (string_of_int(f) ++
    (string_of_int(g) ++ (string_of_int(h) ++ (string_of_int(i) ++ string_of_int(j)))))))))

  | DomesticParen =>
    "(" ++
    (string_of_int(a) ++
    (string_of_int(b) ++
    (string_of_int(c) ++
    (") " ++
    (string_of_int(d) ++
    (string_of_int(e) ++
    (string_of_int(f) ++
    ("-" ++
    (string_of_int(g) ++
    (string_of_int(h) ++ (string_of_int(i) ++ string_of_int(j))))))))))))

  | International =>
    "+1-" ++
    (string_of_int(a) ++
    (string_of_int(b) ++
    (string_of_int(c) ++
    ("-" ++
    (string_of_int(d) ++
    (string_of_int(e) ++
    (string_of_int(f) ++
    ("-" ++
    (string_of_int(g) ++
    (string_of_int(h) ++ (string_of_int(i) ++ string_of_int(j))))))))))))

  | InternationalShort =>
    "+1" ++
    (string_of_int(a) ++
    (string_of_int(b) ++
    (string_of_int(c) ++
    (string_of_int(d) ++
    (string_of_int(e) ++
    (string_of_int(f) ++
    (string_of_int(g) ++
    (string_of_int(h) ++ (string_of_int(i) ++ string_of_int(j))))))))))
  }
}
