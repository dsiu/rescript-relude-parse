open Jest
open Expect
open TestUtils
open P.Infix

describe("ReludeParse_Parser", () => {
  test("runParser success", () => expect(P.runParser("9", P.anyDigit))->toEqual(Ok("9")))

  test("runParser failure", () => {
    let expectedError: P.ParseError.t = ParseError("Expected a digit, but found character 'a'")
    expect(P.runParser("a", P.anyDigit))->toEqual(Error(expectedError))
  })

  test("pure", () => {
    let actual: Belt.Result.t<P.success<int>, P.error> =
      P.pure(42) |> P.unParser({pos: 0, str: "whatever"})
    let expected: Belt.Result.t<P.success<int>, P.error> = Ok({
      result: 42,
      suffix: {
        pos: 0,
        str: "whatever",
      },
    })
    expect(actual)->toEqual(expected)
  })

  test("unit", () => testParse(P.unit, "whatever", (), {pos: 0, str: "whatever"}))

  test("fail", () => {
    let actual = P.fail("Fail!") |> P.unParser({pos: 0, str: "whatever"})
    let expected: Belt.Result.t<P.success<_>, P.error> = Error({
      error: ParseError("Fail!"),
      pos: 0,
    })
    expect(actual)->toEqual(expected)
  })

  test("tries success", () =>
    testParse(P.tries(P.times(3, P.anyDigit)), "012", list{"0", "1", "2"}, {pos: 3, str: "012"})
  )

  test("without tries, error position is position of error", () =>
    testParseFail(P.times(3, P.anyDigit), "01a", 2)
  )

  test("with tries, error position is the starting position", () =>
    testParseFail(P.tries(P.times(3, P.anyDigit)), "01a", 0)
  )

  test("withError", () =>
    testParseFailWithMessage(P.withError("Fail!", P.anyDigit), "a", 0, "Fail!")
  )

  test("flipWithError", () =>
    testParseFailWithMessage(P.flipWithError(P.anyDigit, "Fail!"), "a", 0, "Fail!")
  )

  test("<?> (withError operator)", () =>
    testParseFailWithMessage(\"<?>"(P.anyDigit, "Fail!"), "a", 0, "Fail!")
  )

  test("map/<$>/<#>", () =>
    testParse(\"<#>"(P.anyDigit, int_of_string), "9", 9, {pos: 1, str: "9"})
  )

  test("tap", () => {
    let resultRef = ref("")
    let posStringBeforeRef = ref(P.PosString.make(-1, ""))
    let posStringAfterRef = ref(P.PosString.make(-1, ""))

    let result =
      P.anyDigit
      |> P.tap(
        (result, posStringBefore, posStringAfter) => {
          resultRef := result
          posStringBeforeRef := posStringBefore
          posStringAfterRef := posStringAfter
        },
      )
      |> P.runParser("1")

    expect((
      resultRef.contents,
      posStringBeforeRef.contents,
      posStringAfterRef.contents,
      result,
    ))->toEqual(("1", P.PosString.make(0, "1"), P.PosString.make(1, "1"), Ok("1")))
  })

  test("tapLog", () => expect(P.anyDigit |> P.tapLog |> P.runParser("1") |> ignore)->toEqual())

  test("apply/<*>", () =>
    testParse(\"<*>"(P.pure(int_of_string), P.anyDigit), "9", 9, {pos: 1, str: "9"})
  )

  test("<^> (tuple2 operator)", () =>
    testParse(\"<^>"(P.anyDigit, P.anyDigit), "01", ("0", "1"), {pos: 2, str: "01"})
  )

  test("mapN (applicative extensions)", () =>
    testParse(
      P.map3((a, b, c) => (a, b, c), P.anyDigit, P.anyDigit, P.anyDigit),
      "012",
      ("0", "1", "2"),
      {pos: 3, str: "012"},
    )
  )

  test("tupleN (applicative extensions)", () =>
    testParse(
      P.tuple3(P.anyDigit, P.anyDigit, P.anyDigit),
      "012",
      ("0", "1", "2"),
      {pos: 3, str: "012"},
    )
  )

  test("mapTupleN (applicative extensions)", () =>
    testParse(
      (P.anyDigit, P.anyDigit, P.anyDigit) |> P.mapTuple3((a, b, c) => (a, b, c)),
      "012",
      ("0", "1", "2"),
      {pos: 3, str: "012"},
    )
  )

  test("*>", () =>
    testParse(\"<*"(\"*>"(P.anyDigit, P.anyDigit), P.eof), "12", "2", {pos: 2, str: "12"})
  )

  test("<*", () =>
    testParse(\"<*"(\"<*"(P.anyDigit, P.anyDigit), P.eof), "12", "1", {pos: 2, str: "12"})
  )

  test("bind/flatMap/>>=", () => {
    let twoDigits = \">>="(P.anyDigit, d1 => \"<#>"(P.anyDigit, d2 => (d1, d2)))
    testParse(twoDigits, "01", ("0", "1"), {pos: 2, str: "01"})
  })

  test(">>= nested", () => {
    let p = \">>="(
      P.anyDigit,
      a => \">>="(P.anyDigit, b => \">>="(P.anyDigit, c => \"<#>"(P.anyDigit, d => (a, b, c, d)))),
    )
    testParse(p, "0123", ("0", "1", "2", "3"), {pos: 4, str: "0123"})
  })

  test(">>= conditional", () => {
    let p = \">>="(P.anyDigitAsInt, count => P.times(count, P.anyChar))
    testParse(p, "3abcdef", list{"a", "b", "c"}, {pos: 4, str: "3abcdef"})
  })

  test("throwError", () => {
    let actual = P.throwError(P.ParseError.make("hi")) |> P.unParser({pos: 0, str: "whatever"})
    let expected: Belt.Result.t<P.success<_>, P.error> = Error({
      error: ParseError("hi"),
      pos: 0,
    })
    expect(actual)->toEqual(expected)
  })

  test("catchError", () =>
    testParse(
      \"<?>"(P.anyDigit, "a") |> P.catchError((ParseError(msg)) => P.str(msg)),
      "a",
      "a",
      {pos: 1, str: "a"},
    )
  )

  test("alt/<|> first success", () =>
    testParse(\"<|>"(P.anyDigit, P.anyAlpha), "2", "2", {str: "2", pos: 1})
  )

  test("alt/<|> second success", () =>
    testParse(\"<|>"(P.anyDigit, P.anyAlpha), "a", "a", {str: "a", pos: 1})
  )

  test("alt/<|> failure", () => testParseFail(\"<|>"(P.anyDigit, P.anyAlpha), "!", 0))

  test("alt/<|> failure 2", () =>
    testParseFail(\"<|>"(\"<|>"(P.str("abc"), P.str("def")), P.str("ghi")), "!!!!", 0)
  )

  test("altLazy first success", () =>
    testParse(P.altLazy(P.anyDigit, () => P.anyAlpha), "2", "2", {str: "2", pos: 1})
  )

  test("altLazy second success", () =>
    testParse(P.altLazy(P.anyDigit, () => P.anyAlpha), "a", "a", {str: "a", pos: 1})
  )

  test("altLazy failure", () => testParseFail(P.altLazy(P.anyDigit, () => P.anyAlpha), "!", 0))

  test("orElse first success", () =>
    testParse(P.anyDigit |> P.orElse(~fallback=P.anyDigit), "2", "2", {str: "2", pos: 1})
  )

  test("orElse second success", () =>
    testParse(P.anyDigit |> P.orElse(~fallback=P.anyAlpha), "a", "a", {str: "a", pos: 1})
  )

  test("orElse failure", () => testParseFail(P.anyDigit |> P.orElse(~fallback=P.anyDigit), "!", 0))

  test("orElseLazy first success", () =>
    testParse(P.anyDigit |> P.orElseLazy(~fallback=() => P.anyDigit), "2", "2", {str: "2", pos: 1})
  )

  test("orElseLazy second success", () =>
    testParse(P.anyDigit |> P.orElseLazy(~fallback=() => P.anyAlpha), "a", "a", {str: "a", pos: 1})
  )

  test("orElseLazy failure", () =>
    testParseFail(P.anyDigit |> P.orElseLazy(~fallback=() => P.anyDigit), "!", 0)
  )

  test("lookAhead preserves the position", () =>
    testParse(P.lookAhead(P.times(3, P.anyDigit)), "012", list{"0", "1", "2"}, {str: "012", pos: 0})
  )

  test("lookAhead preserves the position mid-parse", () =>
    testParse(
      \">>="(
        P.times(3, P.anyDigit),
        first => P.lookAhead(\"<#>"(P.times(3, P.anyDigit), second => (first, second))),
      ),
      "012345",
      (list{"0", "1", "2"}, list{"3", "4", "5"}),
      {str: "012345", pos: 3},
    )
  )

  test("lookAhead failure", () => testParseFail(P.lookAhead(P.anyDigit), "a", 0))

  test("lookAheadNot success", () =>
    testParse(
      \"<^>"(P.anyAlpha, P.lookAheadNot(P.str("bbb"))),
      "a999",
      ("a", ()),
      {pos: 1, str: "a999"},
    )
  )

  test("lookAheadNot failure", () =>
    testParseFail(\"<^>"(P.anyAlpha, P.lookAheadNot(P.str("bbb"))), "abbb", 1)
  )

  test("many success", () =>
    testParse(
      P.many(P.anyDigit),
      "0123456789",
      list{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"},
      {pos: 10, str: "0123456789"},
    )
  )

  test("many non-spaces", () =>
    testParse(P.many(P.str("a")), "aaa", list{"a", "a", "a"}, {pos: 3, str: "aaa"})
  )

  test("many spaces", () =>
    testParse(P.many(P.str(" ")), "   ", list{" ", " ", " "}, {pos: 3, str: "   "})
  )

  test("many with eof success", () =>
    testParse(
      \"<*"(P.many(P.anyDigit), P.eof),
      "0123456789",
      list{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"},
      {pos: 10, str: "0123456789"},
    )
  )

  test("many with eof failure", () =>
    testParseFail(\"<*"(P.many(P.anyDigit), P.eof), "0123456789abc", 10)
  )

  test("many with extra success", () =>
    testParse(
      P.many(P.anyDigit),
      "0123456789abc",
      list{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"},
      {pos: 10, str: "0123456789abc"},
    )
  )

  test("many with empty success", () =>
    testParse(P.many(P.anyDigit), "", list{}, {pos: 0, str: ""})
  )

  test("many with non-match success", () =>
    testParse(P.many(P.anyDigit), "abc", list{}, {pos: 0, str: "abc"})
  )

  test("many1 success", () =>
    testParse(
      P.many1(P.anyDigit),
      "0123456789",
      Relude.Nel.make("0", list{"1", "2", "3", "4", "5", "6", "7", "8", "9"}),
      {pos: 10, str: "0123456789"},
    )
  )

  test("many1 with eof success", () =>
    testParse(
      \"<*"(P.many1(P.anyDigit), P.eof),
      "0123456789",
      Relude.Nel.make("0", list{"1", "2", "3", "4", "5", "6", "7", "8", "9"}),
      {pos: 10, str: "0123456789"},
    )
  )

  test("many1 with eof failure", () =>
    testParseFail(\"<*"(P.many1(P.anyDigit), P.eof), "0123456789abc", 10)
  )

  test("many1 with extra success", () =>
    testParse(
      P.many1(P.anyDigit),
      "0123456789abc",
      Relude.Nel.make("0", list{"1", "2", "3", "4", "5", "6", "7", "8", "9"}),
      {pos: 10, str: "0123456789abc"},
    )
  )

  test("many1 with empty failure", () => testParseFail(P.many1(P.anyDigit), "", 0))

  test("many1 with non-match failure", () => testParseFail(P.many1(P.anyDigit), "abc", 0))

  test("times 0 empty string", () =>
    testParse(P.times(0, P.anyDigit), "", list{}, {pos: 0, str: ""})
  )

  test("times 0 valid string", () =>
    testParse(P.times(0, P.anyDigit), "123", list{}, {pos: 0, str: "123"})
  )

  test("times 0 invalid string", () =>
    testParse(P.times(0, P.anyDigit), "abc", list{}, {pos: 0, str: "abc"})
  )

  test("times 1 empty string", () => testParseFail(P.times(1, P.anyDigit), "", 0))

  test("times 1 valid string", () =>
    testParse(P.times(1, P.anyDigit), "123", list{"1"}, {pos: 1, str: "123"})
  )

  test("times 1 invalid string", () => testParseFail(P.times(1, P.anyDigit), "abc", 0))

  test("times 2 empty string", () => testParseFail(P.times(2, P.anyDigit), "", 0))

  test("times 2 valid string", () =>
    testParse(P.times(2, P.anyDigit), "123", list{"1", "2"}, {pos: 2, str: "123"})
  )

  test("times 2 invalid string", () => testParseFail(P.times(2, P.anyDigit), "abc", 0))

  test("times 2 invalid string 2", () => testParseFail(P.times(2, P.anyDigit), "0ab", 1))

  test("times2", () => testParse(P.times2(P.anyDigit), "12", ("1", "2"), {pos: 2, str: "12"}))

  test("times3", () =>
    testParse(P.times3(P.anyDigit), "123", ("1", "2", "3"), {pos: 3, str: "123"})
  )

  test("times4", () =>
    testParse(P.times4(P.anyDigit), "1234", ("1", "2", "3", "4"), {pos: 4, str: "1234"})
  )

  test("times5", () =>
    testParse(P.times5(P.anyDigit), "12345", ("1", "2", "3", "4", "5"), {pos: 5, str: "12345"})
  )

  test("timesMinMax exact min", () =>
    testParse(P.anyDigit |> P.timesMinMax(2, 5), "01abc", list{"0", "1"}, {pos: 2, str: "01abc"})
  )

  test("timesMinMax exact max", () =>
    testParse(
      P.anyDigit |> P.timesMinMax(2, 5),
      "0123abc",
      list{"0", "1", "2", "3"},
      {pos: 4, str: "0123abc"},
    )
  )

  test("timesMinMax over max", () =>
    testParse(
      P.anyDigit |> P.timesMinMax(2, 5),
      "0123456789",
      list{"0", "1", "2", "3", "4"},
      {pos: 5, str: "0123456789"},
    )
  )

  test("timesMinMax partial", () =>
    testParse(
      P.anyDigit |> P.timesMinMax(2, 5),
      "012abc",
      list{"0", "1", "2"},
      {pos: 3, str: "012abc"},
    )
  )

  test("timesMinMax failure", () => testParseFail(P.anyDigit |> P.timesMinMax(2, 5), "0abc", 1))

  test("timesMin exact min", () =>
    testParse(P.anyDigit |> P.timesMin(2), "01abc", list{"0", "1"}, {pos: 2, str: "01abc"})
  )

  test("timesMin full", () =>
    testParse(
      P.anyDigit |> P.timesMin(2),
      "01234abc",
      list{"0", "1", "2", "3", "4"},
      {pos: 5, str: "01234abc"},
    )
  )

  test("timesMin fail", () => testParseFail(P.anyDigit |> P.timesMin(2), "0abc", 1))

  test("timesMax empty", () =>
    testParse(P.anyDigit |> P.timesMax(3), "", list{}, {pos: 0, str: ""})
  )

  test("timesMax partial", () =>
    testParse(P.anyDigit |> P.timesMax(3), "01abc", list{"0", "1"}, {pos: 2, str: "01abc"})
  )

  test("timesMax exact max", () =>
    testParse(P.anyDigit |> P.timesMax(3), "012abc", list{"0", "1", "2"}, {pos: 3, str: "012abc"})
  )

  test("timesMax over max", () =>
    testParse(
      P.anyDigit |> P.timesMax(3),
      "01234abc",
      list{"0", "1", "2"},
      {pos: 3, str: "01234abc"},
    )
  )

  test("between simple", () =>
    testParse(
      P.between(P.leftParen, P.rightParen, \"<#>"(P.many(P.anyDigit), Relude.List.String.join)),
      "(123)",
      "123",
      {pos: 5, str: "(123)"},
    )
  )

  test("between extra spaces 1", () =>
    testParse(
      P.between(P.leftParen, P.rightParen, \"<*"(\"*>"(P.wsStr, P.anyNonEmptyDigits), P.wsStr)),
      "( 123  )",
      "123",
      {pos: 8, str: "( 123  )"},
    )
  )

  test("between extra spaces 2", () =>
    testParse(
      P.between(
        \"<*"(\"*>"(P.ws, P.leftParen), P.ws),
        \"<*"(\"*>"(P.ws, P.rightParen), P.ws),
        P.anyNonEmptyDigits,
      ),
      "( 123  )",
      "123",
      {pos: 8, str: "( 123  )"},
    )
  )

  test("orDefault success", () =>
    testParse(P.anyDigit |> P.orDefault("!"), "9", "9", {pos: 1, str: "9"})
  )

  test("orDefault default", () =>
    testParse(P.anyDigit |> P.orDefault("!"), "x", "!", {pos: 0, str: "x"})
  ) // TODO: not sure if pos should be advanced here?

  test("orUnit hit", () => testParse(P.anyDigit |> P.orUnit, "3", (), {pos: 1, str: "3"}))

  test("orUnit miss", () => testParse(P.anyDigit |> P.orUnit, "a", (), {pos: 0, str: "a"}))

  test("opt hit", () => testParse(P.anyDigit |> P.opt, "3", Some("3"), {pos: 1, str: "3"}))

  test("opt miss", () => testParse(P.anyDigit |> P.opt, "a", None, {pos: 0, str: "a"}))

  test("sepBy empty", () =>
    testParse(P.anyDigit |> P.sepBy(P.str(",")), "", list{}, {pos: 0, str: ""})
  )

  test("sepBy no trailing", () =>
    testParse(
      P.anyDigit |> P.sepBy(P.str(",")),
      "1,2,3",
      list{"1", "2", "3"},
      {pos: 5, str: "1,2,3"},
    )
  )

  test("sepBy trailing", () => testParseFail(P.anyDigit |> P.sepBy(P.str(",")), "1,2,3,", 6))

  test("sepBy1 no trailing", () =>
    testParse(
      P.anyDigit |> P.sepBy1(P.str(",")),
      "1,2,3",
      Relude.Nel.make("1", list{"2", "3"}),
      {pos: 5, str: "1,2,3"},
    )
  )

  test("sepBy1 trailing", () => testParseFail(P.anyDigit |> P.sepBy1(P.str(",")), "1,2,3,", 6))

  test("sepByOptEnd no trailing", () =>
    testParse(
      P.anyDigit |> P.sepByOptEnd(P.str(",")),
      "1,2,3",
      list{"1", "2", "3"},
      {pos: 5, str: "1,2,3"},
    )
  )

  test("sepByOptEnd trailing", () =>
    testParse(
      P.anyDigit |> P.sepByOptEnd(P.str(",")),
      "1,2,3,",
      list{"1", "2", "3"},
      {pos: 6, str: "1,2,3,"},
    )
  )

  test("sepByOptEnd1 no trailing", () =>
    testParse(
      P.anyDigit |> P.sepByOptEnd1(P.str(",")),
      "1,2,3",
      Relude.Nel.make("1", list{"2", "3"}),
      {pos: 5, str: "1,2,3"},
    )
  )

  test("sepByOptEnd1 trailing", () =>
    testParse(
      P.anyDigit |> P.sepByOptEnd1(P.str(",")),
      "1,2,3,",
      Relude.Nel.make("1", list{"2", "3"}),
      {pos: 6, str: "1,2,3,"},
    )
  )

  test("sepByWithEnd no trailing", () =>
    testParseFail(P.anyDigit |> P.sepByWithEnd(P.str(",")), "1,2,3", 5)
  )

  test("sepByWithEnd trailing", () =>
    testParse(
      P.anyDigit |> P.sepByWithEnd(P.str(",")),
      "1,2,3,",
      list{"1", "2", "3"},
      {pos: 6, str: "1,2,3,"},
    )
  )

  test("sepByWithEnd1 no trailing", () =>
    testParseFail(P.anyDigit |> P.sepByWithEnd1(P.str(",")), "1,2,3", 5)
  )

  test("sepByWithEnd1 trailing", () =>
    testParse(
      P.anyDigit |> P.sepByWithEnd1(P.str(",")),
      "1,2,3,",
      Relude.Nel.make("1", list{"2", "3"}),
      {pos: 6, str: "1,2,3,"},
    )
  )

  test("chainr", () =>
    testParse(
      P.chainr(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), "", P.anyDigit),
      "1+2+3",
      "(1+(2+3))",
      {pos: 5, str: "1+2+3"},
    )
  )

  test("chainr empty", () =>
    testParse(
      P.chainr(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), "", P.anyDigit),
      "",
      "",
      {pos: 0, str: ""},
    )
  )

  test("chainr1", () =>
    testParse(
      P.chainr1(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), P.anyDigit),
      "1+2+3",
      "(1+(2+3))",
      {pos: 5, str: "1+2+3"},
    )
  )

  test("chainr1 empty", () =>
    testParseFail(
      P.chainr1(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), P.anyDigit),
      "",
      0,
    )
  )

  test("chainl", () =>
    testParse(
      P.chainl(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), "", P.anyDigit),
      "1+2+3",
      "((1+2)+3)",
      {pos: 5, str: "1+2+3"},
    )
  )

  test("chainl empty", () =>
    testParse(
      P.chainl(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), "", P.anyDigit),
      "",
      "",
      {pos: 0, str: ""},
    )
  )

  test("chainl1", () =>
    testParse(
      P.chainl1(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), P.anyDigit),
      "1+2+3",
      "((1+2)+3)",
      {pos: 5, str: "1+2+3"},
    )
  )

  test("chainl1 empty", () =>
    testParseFail(
      P.chainl1(\"$>"(P.str("+"), (a, b) => "(" ++ (a ++ ("+" ++ (b ++ ")")))), P.anyDigit),
      "",
      0,
    )
  )

  test("anyOf first", () =>
    testParse(P.anyOf(list{P.str("a"), P.str("b"), P.str("c")}), "a", "a", {pos: 1, str: "a"})
  )

  test("anyOf second", () =>
    testParse(P.anyOf(list{P.str("a"), P.str("b"), P.str("c")}), "b", "b", {pos: 1, str: "b"})
  )

  test("anyOf fail", () => testParseFail(P.anyOf(list{P.str("a"), P.str("b"), P.str("c")}), "d", 0))

  // manyUntilWithEnd

  test("manyUntilWithEnd full success", () =>
    testParse(
      P.anyDigit |> P.manyUntilWithEnd(P.str("!")),
      "123!",
      (list{"1", "2", "3"}, "!"),
      {pos: 4, str: "123!"},
    )
  )

  test("manyUntilWithEnd empty success", () =>
    testParse(P.anyDigit |> P.manyUntilWithEnd(P.str("!")), "!", (list{}, "!"), {pos: 1, str: "!"})
  )

  test("manyUntilWithEnd failure", () =>
    testParseFail(P.anyDigit |> P.manyUntilWithEnd(P.str("!")), "123", 3)
  )

  // many1UntilWithEnd

  test("many1UntilWithEnd full success", () =>
    testParse(
      P.anyDigit |> P.many1UntilWithEnd(P.str("!")),
      "123!",
      (Relude.Nel.make("1", list{"2", "3"}), "!"),
      {pos: 4, str: "123!"},
    )
  )

  test("many1UntilWithEnd empty failure", () =>
    testParseFail(P.anyDigit |> P.many1UntilWithEnd(P.str("!")), "!", 0)
  )

  test("many1UntilWithEnd failure", () =>
    testParseFail(P.anyDigit |> P.many1UntilWithEnd(P.str("!")), "123", 3)
  )

  // manyUntil

  test("manyUntil full success", () =>
    testParse(
      P.anyDigit |> P.manyUntil(P.str("!")),
      "123!",
      list{"1", "2", "3"},
      {pos: 4, str: "123!"},
    )
  )

  test("manyUntil empty success", () =>
    testParse(P.anyDigit |> P.manyUntil(P.str("!")), "!", list{}, {pos: 1, str: "!"})
  )

  test("manyUntil failure", () => testParseFail(P.anyDigit |> P.manyUntil(P.str("!")), "123", 3))

  // many1Until

  test("many1Until full success", () =>
    testParse(
      P.anyDigit |> P.many1Until(P.str("!")),
      "123!",
      Relude.Nel.make("1", list{"2", "3"}),
      {pos: 4, str: "123!"},
    )
  )

  test("many1Until empty failure", () =>
    testParseFail(P.anyDigit |> P.many1Until(P.str("!")), "!", 0)
  )

  test("many1Until failure", () => testParseFail(P.anyDigit |> P.many1Until(P.str("!")), "123", 3))

  // manyUntilPeekWithEnd

  test("manyUntilPeekWithEnd full success", () =>
    testParse(
      P.anyDigit |> P.manyUntilPeekWithEnd(P.str("!")),
      "123!",
      (list{"1", "2", "3"}, "!"),
      {pos: 3, str: "123!"},
    )
  )

  test("manyUntilPeekWithEnd empty success", () =>
    testParse(
      P.anyDigit |> P.manyUntilPeekWithEnd(P.str("!")),
      "!",
      (list{}, "!"),
      {pos: 0, str: "!"},
    )
  )

  test("manyUntilPeekWithEnd failure", () =>
    testParseFail(P.anyDigit |> P.manyUntilPeekWithEnd(P.str("!")), "123", 3)
  )

  // many1UntilPeekWithEnd

  test("many1UntilPeekWithEnd full success", () =>
    testParse(
      P.anyDigit |> P.many1UntilPeekWithEnd(P.str("!")),
      "123!",
      (Relude.Nel.make("1", list{"2", "3"}), "!"),
      {pos: 3, str: "123!"},
    )
  )

  test("many1UntilPeekWithEnd empty failure", () =>
    testParseFail(P.anyDigit |> P.many1UntilPeekWithEnd(P.str("!")), "!", 0)
  )

  test("many1UntilPeekWithEnd failure", () =>
    testParseFail(P.anyDigit |> P.many1UntilPeekWithEnd(P.str("!")), "123", 3)
  )

  // manyUntilPeek

  test("manyUntilPeek full success", () =>
    testParse(
      P.anyDigit |> P.manyUntilPeek(P.str("!")),
      "123!",
      list{"1", "2", "3"},
      {pos: 3, str: "123!"},
    )
  )

  test("manyUntilPeek empty success", () =>
    testParse(P.anyDigit |> P.manyUntilPeek(P.str("!")), "!", list{}, {pos: 0, str: "!"})
  )

  test("manyUntilPeek failure", () =>
    testParseFail(P.anyDigit |> P.manyUntilPeek(P.str("!")), "123", 3)
  )

  // many1UntilPeek

  test("many1UntilPeek full success", () =>
    testParse(
      P.anyDigit |> P.many1UntilPeek(P.str("!")),
      "123!",
      Relude.Nel.make("1", list{"2", "3"}),
      {pos: 3, str: "123!"},
    )
  )

  test("many1UntilPeek empty failure", () =>
    testParseFail(P.anyDigit |> P.many1UntilPeek(P.str("!")), "!", 0)
  )

  test("many1UntilPeek failure", () =>
    testParseFail(P.anyDigit |> P.many1UntilPeek(P.str("!")), "123", 3)
  )

  // filter

  test("filter success", () =>
    testParse(
      \"<?>"(P.anyInt |> P.filter(i => i <= 255), "Expected an int less than 255"),
      "255",
      255,
      {pos: 3, str: "255"},
    )
  )

  test("filter failure", () =>
    testParseFailWithMessage(
      \"<?>"(P.anyInt |> P.filter(i => i <= 255), "Expected an int less than 255"),
      "256",
      0,
      "Expected an int less than 255",
    )
  )

  test("getSome success", () =>
    testParse(P.opt(P.anyDigit) |> P.getSome, "9ab", "9", {pos: 1, str: "9ab"})
  )

  test("getSome failure", () => testParseFail(P.opt(P.anyDigit) |> P.getSome, "abc", 0))

  test("getNonEmptyStr success", () =>
    testParse(P.anyDigit |> P.getNonEmptyStr, "9ab", "9", {pos: 1, str: "9ab"})
  )

  test("getNonEmptyStr failure", () =>
    testParseFail(\"<#>"(P.anyDigit, _ => "") |> P.getNonEmptyStr, "9ab", 1)
  )

  test("getFst", () =>
    testParse(P.pure((42, "a")) |> P.getFst, "whatever", 42, {pos: 0, str: "whatever"})
  )

  test("getSnd", () =>
    testParse(P.pure((42, "a")) |> P.getSnd, "whatever", "a", {pos: 0, str: "whatever"})
  )

  test("eof empty string", () => testParse(P.eof, "", (), {pos: 0, str: ""}))

  test("eof non-empty success", () =>
    testParse(\"<*"(P.anyChar, P.eof), "a", "a", {pos: 1, str: "a"})
  )

  test("eof non-empty fail", () => testParseFail(\"<*"(P.anyChar, P.eof), "ab", 1))

  let goodChars = list{"a", "0", "!", ",", " "}
  let badChars = list{""}

  testAll("anyChar success", goodChars, input =>
    testParse(P.anyChar, input, input, {pos: 1, str: input})
  )

  testAll("anyChar fail", badChars, input => testParseFail(P.anyChar, input, 0))

  test("notChar success", () => testParse(P.notChar("b"), "abc", "a", {pos: 1, str: "abc"}))

  test("notChar failure match", () => testParseFail(P.notChar("a"), "abc", 0))

  test("notChar failure empty", () => testParseFail(P.notChar("a"), "", 0))

  let goodDigits = list{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}
  let badDigits = list{"a", "!", ",", " ", "/", ":"}

  testAll("anyDigitAsInt success", goodDigits, input =>
    testParse(P.anyDigitAsInt, input, int_of_string(input), {pos: 1, str: input})
  )

  testAll("anyDigitAsInt fail", badDigits, input => testParseFail(P.anyDigitAsInt, input, 0))

  testAll("anyDigit success", goodDigits, input =>
    testParse(P.anyDigit, input, input, {pos: 1, str: input})
  )

  testAll("anyDigit fail", badDigits, input => testParseFail(P.anyDigit, input, 0))

  testAll("anyNonZeroDigit", list{"1", "2", "3", "4", "5", "6", "7", "8", "9"}, input =>
    testParse(P.anyNonZeroDigit, input, input, {pos: 1, str: input})
  )

  testAll("anyNonZeroDigit fail", list{"/", "0", "a", "!", ":"}, input =>
    testParseFail(P.anyNonZeroDigit, input, 0)
  )

  testAll("anyNonZeroDigitAsInt", list{"1", "2", "3", "4", "5", "6", "7", "8", "9"}, input =>
    testParse(P.anyNonZeroDigitAsInt, input, int_of_string(input), {pos: 1, str: input})
  )

  testAll("anyNonZeroDigitAsInt fail", list{"0", "a", "!"}, input =>
    testParseFail(P.anyNonZeroDigitAsInt, input, 0)
  )

  testAll(
    "anyUnsignedInt success",
    list{("0", 0, 1), ("1", 1, 1), ("2", 2, 1), ("12", 12, 2), ("12345", 12345, 5)},
    ((input, expected, pos)) =>
      testParse(\"<*"(P.anyUnsignedInt, P.eof), input, expected, {pos, str: input}),
  )

  testAll(
    "anyUnsignedInt fail",
    list{
      ("-1", 0),
      ("-0", 0),
      ("a", 0),
      ("!", 0),
      (".0", 0),
      ("0.", 1),
      ("00", 1),
      ("01", 1),
      ("002", 1),
    },
    ((input, pos)) => testParseFail(\"<*"(P.anyUnsignedInt, P.eof), input, pos),
  )

  testAll(
    "anyPositiveInt success",
    list{
      ("0", 0, 1),
      ("1", 1, 1),
      ("2", 2, 1),
      ("12", 12, 2),
      ("12345", 12345, 5),
      ("+0", 0, 2),
      ("+1", 1, 2),
      ("+2", 2, 2),
      ("+12", 12, 3),
      ("+12345", 12345, 6),
    },
    ((input, expected, pos)) =>
      testParse(\"<*"(P.anyPositiveInt, P.eof), input, expected, {pos, str: input}),
  )

  testAll(
    "anyPositiveInt fail",
    list{
      ("-1", 0),
      ("-0", 0),
      ("a", 0),
      ("!", 0),
      (".0", 0),
      ("0.", 1),
      ("00", 1),
      ("01", 1),
      ("002", 1),
      ("+00", 2),
      ("+01", 2),
      ("+002", 2),
    },
    ((input, pos)) => testParseFail(\"<*"(P.anyPositiveInt, P.eof), input, pos),
  )

  testAll(
    "anyNegativeInt success",
    list{("-0", 0, 2), ("-1", -1, 2), ("-2", -2, 2), ("-12", -12, 3), ("-12345", -12345, 6)},
    ((input, expected, pos)) =>
      testParse(\"<*"(P.anyNegativeInt, P.eof), input, expected, {pos, str: input}),
  )

  testAll(
    "anyNegativeInt fail",
    list{
      ("1", 0),
      ("0", 0),
      ("a", 0),
      ("!", 0),
      (".0", 0),
      ("0.", 0),
      ("-.0", 1),
      ("-0.", 2),
      ("-00", 2),
      ("-01", 2),
      ("-002", 2),
    },
    ((input, pos)) => testParseFail(\"<*"(P.anyNegativeInt, P.eof), input, pos),
  )

  testAll(
    "anyInt success",
    list{
      ("0", 0, 1),
      ("1", 1, 1),
      ("2", 2, 1),
      ("12", 12, 2),
      ("12345", 12345, 5),
      ("-0", 0, 2),
      ("-1", -1, 2),
      ("-2", -2, 2),
      ("-12", -12, 3),
      ("-12345", -12345, 6),
    },
    ((input, expected, pos)) =>
      testParse(\"<*"(P.anyInt, P.eof), input, expected, {pos, str: input}),
  )

  testAll(
    "anyInt fail",
    list{
      ("a", 0),
      ("!", 0),
      (".0", 0),
      ("0.", 1),
      ("-0.", 2),
      ("00", 1),
      ("01", 1),
      ("002", 1),
      ("-00", 2),
      ("-01", 2),
      ("-002", 2),
    },
    ((input, pos)) => testParseFail(\"<*"(P.anyInt, P.eof), input, pos),
  )

  testAll(
    "anyDecimal success",
    list{
      ("0", "0", 1),
      ("1", "1", 1),
      ("12", "12", 2),
      (".0", ".0", 2),
      (".00", ".00", 3),
      (".001", ".001", 4),
      ("0.0", "0.0", 3),
      ("0.00", "0.00", 4),
      ("0.000", "0.000", 5),
      ("0.001", "0.001", 5),
      ("1.000", "1.000", 5),
      ("1.002", "1.002", 5),
      ("9.87e3", "9.87e3", 6),
      ("9.87e33", "9.87e33", 7),
      ("9.87e-3", "9.87e-3", 7),
      ("9.87e-33", "9.87e-33", 8),
      ("9.87E3", "9.87E3", 6),
      ("9.87E33", "9.87E33", 7),
      ("9.87E-3", "9.87E-3", 7),
      ("9.87E-33", "9.87E-33", 8),
    },
    ((str, expected, pos)) => testParse(\"<*"(P.anyDecimal, P.eof), str, expected, {pos, str}),
  )

  testAll(
    "anyDecimal failure",
    list{
      ("0.", 1), // This could arguably parse, but it doesn't right now
      ("a", 0),
      ("-a", 1),
      ("!", 0),
      (".0a", 2),
      ("-0.a", 2),
      ("0.a", 1),
      ("1.2e-a", 3),
    },
    ((str, pos)) => testParseFail(\"<*"(P.anyDecimal, P.eof), str, pos),
  )

  testAll("boolTrue", list{("true", true, 4), ("TRUE", true, 4), ("TrUe", true, 4)}, ((
    str,
    exp,
    pos,
  )) => testParse(\"<*"(P.boolTrue, P.eof), str, exp, {str, pos}))

  testAll("boolFalse", list{("false", false, 5), ("FALSE", false, 5), ("fALse", false, 5)}, ((
    str,
    exp,
    pos,
  )) => testParse(\"<*"(P.boolFalse, P.eof), str, exp, {str, pos}))

  testAll(
    "anyBool success",
    list{
      ("true", true, 4),
      ("TRUE", true, 4),
      ("TrUe", true, 4),
      ("false", false, 5),
      ("FALSE", false, 5),
      ("fALse", false, 5),
    },
    ((str, exp, pos)) => testParse(\"<*"(P.anyBool, P.eof), str, exp, {str, pos}),
  )

  testAll("anyBool false", list{("tru", 0), ("fal", 0), ("a", 0)}, ((str, pos)) =>
    testParseFail(\"<*"(P.anyBool, P.eof), str, pos)
  )

  test("anyStr empty", () => testParse(P.anyStr, "", "", {pos: 0, str: ""}))

  test("anyStr non-empty", () =>
    testParse(P.anyStr, "abc123! ", "abc123! ", {pos: 8, str: "abc123! "})
  )

  test("anyNonEmptyStr empty", () => testParseFail(P.anyNonEmptyStr, "", 0))

  test("anyNonEmptyStr non-empty", () =>
    testParse(P.anyNonEmptyStr, "abc123! ", "abc123! ", {pos: 8, str: "abc123! "})
  )

  test("str empty", () => testParse(P.str(""), "", "", {pos: 0, str: ""}))

  test("str simple", () => testParse(P.str("x"), "x", "x", {pos: 1, str: "x"}))

  test("str multiple", () => testParse(P.str("abc"), "abcdef", "abc", {pos: 3, str: "abcdef"}))

  test("str failure", () => testParseFail(P.str("xxx"), "abcdef", 0))

  test("strIgnoreCase", () => testParse(P.strIgnoreCase("Hi"), "hI", "hI", {pos: 2, str: "hI"}))

  test("anyCharBy success", () =>
    testParse(P.anyCharBy(c => c == "x"), "x", "x", {pos: 1, str: "x"})
  )

  test("anyCharBy failure", () => testParseFail(P.anyCharBy(c => c == "x"), "y", 0))

  test("anyOfStr first", () =>
    testParse(P.anyOfStr(list{"a", "b", "c"}), "a", "a", {pos: 1, str: "a"})
  )

  test("anyOfStr second", () =>
    testParse(P.anyOfStr(list{"a", "b", "c"}), "b", "b", {pos: 1, str: "b"})
  )

  test("anyOfStr third", () =>
    testParse(P.anyOfStr(list{"a", "b", "c"}), "c", "c", {pos: 1, str: "c"})
  )

  test("anyOfStr failure", () => testParseFail(P.anyOfStr(list{"a", "b", "c"}), "d", 0))

  test("anyOfStr longer", () =>
    testParse(P.anyOfStr(list{"hi", "bye", "hello"}), "bye", "bye", {pos: 3, str: "bye"})
  )

  test("anyOfStr many", () =>
    testParse(
      P.many(P.anyOfStr(list{"a", "b", "c"})),
      "abcd",
      list{"a", "b", "c"},
      {pos: 3, str: "abcd"},
    )
  )

  test("anyOfStrIgnoreCase", () =>
    testParse(P.anyOfStrIgnoreCase(list{"Hi", "Bye", "Hello"}), "bYE", "bYE", {pos: 3, str: "bYE"})
  )

  test("wsList empty", () => testParse(P.wsList, "", list{}, {str: "", pos: 0}))

  test("wsList single", () => testParse(P.wsList, " ", list{" "}, {str: " ", pos: 1}))

  test("wsList multiple", () =>
    testParse(
      P.wsList,
      "   \t \r \n",
      list{" ", " ", " ", "\t", " ", "\r", " ", "\n"},
      {str: "   \t \r \n", pos: 8},
    )
  )

  test("wsStr empty", () => testParse(P.wsStr, "", "", {str: "", pos: 0}))

  test("wsStr single", () => testParse(P.wsStr, " ", " ", {str: " ", pos: 1}))

  test("wsStr multiple", () =>
    testParse(P.wsStr, "   \t \r \n", "   \t \r \n", {str: "   \t \r \n", pos: 8})
  )

  test("ws empty", () => testParse(P.ws, "", (), {str: "", pos: 0}))

  test("ws single", () => testParse(P.ws, " ", (), {str: " ", pos: 1}))

  test("ws multiple", () => testParse(P.ws, "   \t \r \n", (), {str: "   \t \r \n", pos: 8}))

  test("ws leading", () => {
    let x =
      \"<*>"(\"<$>"((a, b) => a + b, \"*>"(P.ws, P.anyDigitAsInt)), P.anyDigitAsInt) |> P.runParser(
        "   34",
      )
    expect(x)->toEqual(Ok(7))
  })

  test("anyCharNotIn success", () =>
    testParse(P.anyCharNotIn(list{"a", "b", "c"}), "x", "x", {pos: 1, str: "x"})
  )

  test("anyCharNotIn failure", () => testParseFail(P.anyCharNotIn(list{"a", "b", "c"}), "b", 0))

  test("anyCharNotInIgnoreCase success", () =>
    testParse(P.anyCharNotInIgnoreCase(list{"a", "b", "c"}), "x", "x", {pos: 1, str: "x"})
  )

  test("anyCharNotInIgnoreCase failure", () =>
    testParseFail(P.anyCharNotInIgnoreCase(list{"a", "b", "c"}), "B", 0)
  )

  test("anyCharInRange", () =>
    testParse(
      P.many(P.anyCharInRange(98, 100)),
      "bcdef",
      list{"b", "c", "d"},
      {pos: 3, str: "bcdef"},
    )
  )

  let allLowerCaseChars = "abcdefghijklmnopqrstuvwxyz"
  let allUpperCaseChars = allLowerCaseChars |> Relude.String.toUpperCase
  let allAlphaChars = allLowerCaseChars ++ allUpperCaseChars
  let allAlphaDigitChars = allAlphaChars ++ "0123456789"

  testAll("anyLowerCaseChar", allLowerCaseChars |> Relude.String.splitList(~delimiter=""), c =>
    testParse(P.anyLowerCaseChar, c, c, {pos: 1, str: c})
  )

  testAll("anyLowerCaseChar fail", list{"`", "{"}, c => testParseFail(P.anyLowerCaseChar, c, 0))

  testAll("anyUpperCaseChar", allUpperCaseChars |> Relude.String.splitList(~delimiter=""), c =>
    testParse(P.anyUpperCaseChar, c, c, {pos: 1, str: c})
  )

  testAll("anyUpperCaseChar fail", list{"@", "["}, c => testParseFail(P.anyUpperCaseChar, c, 0))

  testAll("anyAlpha", allAlphaChars |> Relude.String.splitList(~delimiter=""), c =>
    testParse(P.anyAlpha, c, c, {pos: 1, str: c})
  )

  testAll("anyAlphaOrDigit", allAlphaDigitChars |> Relude.String.splitList(~delimiter=""), c =>
    testParse(P.anyAlphaOrDigit, c, c, {pos: 1, str: c})
  )

  testAll(
    "anyHexDigit valid",
    list{
      "0",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "a",
      "b",
      "c",
      "d",
      "e",
      "f",
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
    },
    c => testParse(P.anyHexDigit, c, c, {pos: 1, str: c}),
  )

  testAll("anyHexDigit invalid", list{"", " ", "!", "g", "G"}, c =>
    testParseFail(P.anyHexDigit, c, 0)
  )

  testAll(
    "regex various successful",
    list{
      ("aaab", %re("/a+/"), "aaa", 3),
      ("aaab", %re("/a{1,2}/"), "aa", 2),
      ("aaabbb", %re("/a+b+/"), "aaabbb", 6),
      ("aaabbb", %re("/a+b+c*/"), "aaabbb", 6),
      ("aaabbbccc", %re("/a+b+c*/"), "aaabbbccc", 9),
      ("aaabbbcccd", %re("/a+b+c*/"), "aaabbbccc", 9),
      ("aAAbBBcCCd", %re("/a+b+c*/i"), "aAAbBBcCC", 9),
    },
    ((str, regex, expected, pos)) => testParse(P.regex(regex), str, expected, {pos, str}),
  )

  test("regex mid-parse", () =>
    testParse(
      \"*>"(P.many1(P.anyDigit), P.regex(%re("/a+b/i"))),
      "123aAab456",
      "aAab",
      {pos: 7, str: "123aAab456"},
    )
  )

  test("regex mid-parse with eof", () =>
    testParse(
      \"<*"(\"<*"(\"*>"(P.many1(P.anyDigit), P.regex(%re("/a+b/i"))), P.many(P.anyDigit)), P.eof),
      "123aAab456",
      "aAab",
      {pos: 10, str: "123aAab456"},
    )
  )

  testAll("regex fail", list{("aaab", %re("/b/"), 0), ("", %re("/a/"), 0)}, ((input, regex, pos)) =>
    testParseFail(P.regex(regex), input, pos)
  )

  testAll(
    "regexStr various successful",
    list{
      ("aaab", "a+", "", "aaa", 3),
      ("aaab", "a{1,2}", "", "aa", 2),
      ("aaabbb", "a+b+", "", "aaabbb", 6),
      ("aaabbb", "a+b+c*", "", "aaabbb", 6),
      ("aaabbbccc", "a+b+c*", "", "aaabbbccc", 9),
      ("aaabbbcccd", "a+b+c*", "", "aaabbbccc", 9),
      ("aAAbBBcCCd", "a+b+c*", "i", "aAAbBBcCC", 9),
    },
    ((str, regexString, flags, expected, pos)) =>
      testParse(P.regexStr(regexString, ~flags), str, expected, {pos, str}),
  )

  test("leftParen", () => testParse(P.leftParen, "(", "(", {pos: 1, str: "("}))

  test("rightParen", () => testParse(P.rightParen, ")", ")", {pos: 1, str: ")"}))

  test("betweenParens", () =>
    testParse(P.betweenParens(P.anyNonEmptyDigits), "(  456  )", "456", {pos: 9, str: "(  456  )"})
  )

  test("leftCurly", () => testParse(P.leftCurly, "{", "{", {pos: 1, str: "{"}))

  test("rightCurly", () => testParse(P.rightCurly, "}", "}", {pos: 1, str: "}"}))

  test("betweenCurlies", () =>
    testParse(P.betweenCurlies(P.anyNonEmptyDigits), "{  456  }", "456", {pos: 9, str: "{  456  }"})
  )

  test("leftSquare", () => testParse(P.leftSquare, "[", "[", {pos: 1, str: "["}))

  test("rightSquare", () => testParse(P.rightSquare, "]", "]", {pos: 1, str: "]"}))

  test("betweenSquares", () =>
    testParse(P.betweenSquares(P.anyNonEmptyDigits), "[  456  ]", "456", {pos: 9, str: "[  456  ]"})
  )

  test("leftAngle", () => testParse(P.leftAngle, "<", "<", {pos: 1, str: "<"}))

  test("rightAngle", () => testParse(P.rightAngle, ">", ">", {pos: 1, str: ">"}))

  test("betweenAngles", () =>
    testParse(P.betweenAngles(P.anyNonEmptyDigits), "<  456  >", "456", {pos: 9, str: "<  456  >"})
  )

  test("betweenDoubleQuotes", () =>
    testParse(
      P.betweenDoubleQuotes(P.anyNonEmptyDigits),
      "\"123\"",
      "123",
      {pos: 5, str: "\"123\""},
    )
  )

  test("betweenSingleQuotes", () =>
    testParse(P.betweenSingleQuotes(P.anyNonEmptyDigits), "'123'", "123", {pos: 5, str: "'123'"})
  )

  test("betweenBackTicks", () =>
    testParse(P.betweenBackTicks(P.anyNonEmptyDigits), "`123`", "123", {pos: 5, str: "`123`"})
  )

  test("cr", () => testParse(P.cr, "\r", "\r", {pos: 1, str: "\r"}))

  test("lf", () => testParse(P.lf, "\n", "\n", {pos: 1, str: "\n"}))

  test("crlf", () => testParse(P.crlf, "\r\n", "\r\n", {pos: 2, str: "\r\n"}))

  test("eol cr", () => testParse(P.eol, "\r", "\r", {pos: 1, str: "\r"}))

  test("eol lf", () => testParse(P.eol, "\n", "\n", {pos: 1, str: "\n"}))

  test("eol crlf", () => testParse(P.eol, "\r\n", "\r\n", {pos: 2, str: "\r\n"}))

  test("eol sepBy", () =>
    testParse(
      P.anyInt |> P.sepBy(P.eol),
      "123\r\n456\r\n789",
      list{123, 456, 789},
      {pos: 13, str: "123\r\n456\r\n789"},
    )
  )

  test("orEOL match", () =>
    testParse(\"<*"(P.anyDigit, P.str(";") |> P.orEOL), "4;", "4", {pos: 2, str: "4;"})
  )

  test("orEOL eol", () =>
    testParse(
      \"<*"(P.anyDigit, P.str(";") |> P.orEOL),
      "4\r\nrest",
      "4",
      {pos: 3, str: "4\r\nrest"},
    )
  )
})
