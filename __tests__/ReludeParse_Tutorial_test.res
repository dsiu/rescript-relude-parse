@@uncurried
@@uncurried.swap

open Jest
open Expect

let jestFail: string => assertion = fail
let parseFail: 'a. string => ReludeParse.Parser.t<'a> = ReludeParse.Parser.fail

describe("Tutorial", () => {
  // This code below is referenced in the README

  test("Running a parser normally (success)", () => {
    open ReludeParse.Parser
    anyDigit->runParser("3", _)->expect->toEqual(Belt.Result.Ok("3"))
  })

  test("Running a parser normally (failure)", () => {
    open ReludeParse.Parser
    anyDigit
    ->runParser("!", _)
    ->expect
    ->toEqual(Belt.Result.Error(ParseError.ParseError("Expected a digit, but found character '!'")))
  })

  test("Running a parser verbosely (success)", () => {
    open ReludeParse.Parser
    anyDigit
    ->unParser({pos: 0, str: "3"}, _)
    ->(
      Relude.Result.fold(
        ({error: ParseError(message)}) => jestFail("Parse should have succeeded: " ++ message),
        success =>
          expect(success)->toEqual({
            result: "3",
            suffix: {
              pos: 1,
              str: "3",
            },
          }),
        _,
      )
    )
  })

  test("mapping the result of a parser - unsafe & impure example", () => {
    open ReludeParse.Parser
    \"<$$>"(
      \"<$$>"(many1(anyDigit), Relude.Nel.foldLeft((acc, v) => acc ++ v, "", _)),
      int_of_string,
    ) // DANGER!
    ->runParser("12345", _)
    ->expect
    ->toEqual(Belt.Result.Ok(12345))
  })

  test("mapping the result of a parser - safe & pure example", () => {
    open ReludeParse.Parser
    \">>="(
      \"<$$>"(\"<*"(many1(anyDigit), eof), Relude.Nel.foldLeft((acc, v) => acc ++ v, "", _)),
      digitString =>
        Relude.Int.fromString(digitString)->(
          Relude.Option.foldLazy(
            () => parseFail("Failed to conver digit string to an int: " ++ digitString),
            pure,
            _,
          )
        ),
    )
    ->runParser("12345", _)
    ->expect
    ->toEqual(Belt.Result.Ok(12345))
  })

  test("tuple2 example", () => {
    open ReludeParse.Parser
    tuple2(anyDigit, anyDigit)->runParser("12", _)->expect->toEqual(Belt.Result.Ok(("1", "2")))
  })

  test("map2 example", () => {
    open ReludeParse.Parser
    map2((a, b) => a + b, anyDigitAsInt, anyDigitAsInt)
    ->runParser("12", _)
    ->expect
    ->toEqual(Belt.Result.Ok(3))
  })

  test("mapTuple2 example", () => {
    open ReludeParse.Parser
    (anyDigitAsInt, anyDigitAsInt)
    ->mapTuple2((a, b) => a + b, _)
    ->runParser("12", _)
    ->expect
    ->toEqual(Belt.Result.Ok(3))
  })

  test("*> example", () => {
    open ReludeParse.Parser
    \"*>"(ws, anyDigit)->runParser("    3", _)->expect->toEqual(Belt.Result.Ok("3"))
  })

  test("<* example", () => {
    open ReludeParse.Parser
    \"<*"(\"<*"(\"*>"(ws, anyDigit), ws), eof)
    ->runParser("    3   ", _)
    ->expect
    ->toEqual(Belt.Result.Ok("3"))
  })

  test("<$> <*> example", () => {
    let add3 = (a, b, c) => a + b + c
    open ReludeParse.Parser
    \"<*>"(
      \"<*>"(
        \"<$>"(
          {
            open Relude.Function
            add3->uncurryFn3
          },
          anyDigitAsInt,
        ),
        anyDigitAsInt,
      ),
      anyDigitAsInt,
    )
    ->runParser("123", _)
    ->expect
    ->toEqual(Belt.Result.Ok(6))
  })

  test("pure example", () => {
    open ReludeParse.Parser
    pure(3)->runParser("abcdef", _)->expect->toEqual(Belt.Result.Ok(3))
  })

  test("Sequence example", () => {
    open ReludeParse.Parser
    anyDigitAsInt
    ->flatMap(count => \"<*"(anyAlpha->(times(count, _)), eof), _)
    ->map(chars => Relude.List.String.join(chars), _)
    ->runParser("3abc", _)
    ->expect
    ->toEqual(Belt.Result.Ok("abc"))
  })

  test("Sequence operator example", () => {
    open ReludeParse.Parser
    \"<$$>"(
      \">>="(anyDigitAsInt, count => \"<*"(times(count, anyAlpha), eof)),
      Relude.List.String.join,
    )
    ->runParser("3abc", _)
    ->expect
    ->toEqual(Belt.Result.Ok("abc"))
  })

  test("Sequence with fail example", () => {
    open ReludeParse.Parser
    \"<$$>"(
      \">>="(
        anyDigitAsInt,
        count =>
          if count >= 5 {
            ReludeParse.Parser.fail("The count cannot be >= 5")
          } else {
            \"<*"(times(count, anyAlpha), eof)
          },
      ),
      Relude.List.String.join,
    )
    ->runParser("9abc", _)
    ->expect
    ->toEqual(Belt.Result.Error(ParseError.ParseError("The count cannot be >= 5")))
  })

  test("<|> left example", () => {
    open ReludeParse.Parser
    \"<|>"(anyDigit, anyAlpha)->runParser("9", _)->expect->toEqual(Belt.Result.Ok("9"))
  })

  test("<|> right example", () => {
    open ReludeParse.Parser
    \"<|>"(anyDigit, anyAlpha)->runParser("c", _)->expect->toEqual(Belt.Result.Ok("c"))
  })

  test("<|> error example", () => {
    open ReludeParse.Parser
    \"<|>"(anyDigit, anyAlpha)
    ->runParser("!", _)
    ->expect
    ->toEqual(
      Belt.Result.Error(ParseError.ParseError("Expected any alphabet letter (upper or lowercase)")),
    )
  })

  test("<|> with tries example", () => {
    open ReludeParse.Parser
    \"<|>"(tries(\"*>"(anyDigit, anyDigit)), \"*>"(anyDigit, anyAlpha))
    ->runParser("9a", _)
    ->expect
    ->toEqual(Belt.Result.Ok("a"))
  })

  test("Custom error example", () => {
    open ReludeParse.Parser
    \"<?>"(many1(anyDigit), "Expected one or more digits")
    ->runParser("abc", _)
    ->expect
    ->toEqual(Belt.Result.Error(ParseError.ParseError("Expected one or more digits")))
  })

  test("IPv4 example 1", () => {
    open ReludeParse.Parser
    (
      \"<*"(anyPositiveShort, str(".")),
      \"<*"(anyPositiveShort, str(".")),
      \"<*"(anyPositiveShort, str(".")),
      anyPositiveShort,
    )
    ->mapTuple4(ReludeParse.IPv4.unsafeFromInts, _)
    ->runParser("127.0.0.1", _)
    ->Relude.Result.map(ReludeParse.IPv4.toTuple, _)
    ->expect
    ->toEqual(Belt.Result.Ok((127, 0, 0, 1)))
  })

  test("IPv4 example 2", () => {
    open ReludeParse.Parser
    \">>="(
      anyPositiveShort,
      a =>
        \">>="(
          str("."),
          _ =>
            \">>="(
              anyPositiveShort,
              b =>
                \">>="(
                  str("."),
                  _ =>
                    \">>="(
                      anyPositiveShort,
                      c =>
                        \">>="(
                          str("."),
                          _ =>
                            \"<$$>"(
                              anyPositiveShort,
                              d => ReludeParse.IPv4.unsafeFromInts(a, b, c, d),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    ->runParser("127.0.0.1", _)
    ->Relude.Result.map(ReludeParse.IPv4.toTuple, _)
    ->expect
    ->toEqual(Belt.Result.Ok((127, 0, 0, 1)))
  })

  test("IPv4 example 3", () => {
    open ReludeParse.Parser
    \"<*>"(
      \"<*"(
        \"<*>"(
          \"<*"(
            \"<*>"(
              \"<*"(
                \"<$>"(
                  {
                    open Relude.Function
                    ReludeParse.IPv4.unsafeFromInts->uncurryFn4
                  },
                  anyPositiveShort,
                ),
                str("."),
              ),
              anyPositiveShort,
            ),
            str("."),
          ),
          anyPositiveShort,
        ),
        str("."),
      ),
      anyPositiveShort,
    )
    ->runParser("127.0.0.1", _)
    ->Relude.Result.map(ReludeParse.IPv4.toTuple, _)
    ->expect
    ->toEqual(Belt.Result.Ok((127, 0, 0, 1)))
  })

  test("IPv4 example 4", () => {
    open ReludeParse.Parser
    anyPositiveShort
    ->sepBy(str("."), _)
    ->\">>="(
      shorts =>
        switch shorts {
        | list{a, b, c, d} => pure(ReludeParse.IPv4.unsafeFromInts(a, b, c, d))
        | _ => parseFail("Expected exactly 4 shorts separated by .")
        },
    )
    ->runParser("127.0.0.1", _)
    ->Relude.Result.map(ReludeParse.IPv4.toTuple, _)
    ->expect
    ->toEqual(Belt.Result.Ok((127, 0, 0, 1)))
  })

  test("CSV example", () => {
    let fieldParser = {
      open ReludeParse.Parser
      \"<$$>"(manyUntilPeek(str(",")->orEOF, anyChar), Relude.List.String.join)
    }

    let csvParser = {
      open ReludeParse.Parser
      fieldParser->(sepBy(str(","), _))
    }

    let actual = csvParser->(ReludeParse.runParser("abc,def", _))
    let expected = Belt.Result.Ok(list{"abc", "def"})
    expect(actual)->toEqual(expected)
  })
})
