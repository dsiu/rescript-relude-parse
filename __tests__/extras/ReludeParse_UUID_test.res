@@uncurried
@@uncurried.swap

open Jest
open Expect
open TestUtils
module UUID = ReludeParse.UUID

describe("ReludeParse_UUID", () => {
  testAll(
    "show",
    list{
      (
        UUID.unsafeFromString("00000000-0000-0000-0000-000000000000"),
        "00000000-0000-0000-0000-000000000000",
      ),
    },
    ((input, exp)) => expect(UUID.show(input))->toEqual(exp),
  )

  testAll(
    "valid",
    list{
      (
        "00000000-0000-0000-0000-000000000000",
        UUID.unsafeFromString("00000000-0000-0000-0000-000000000000"),
      ),
      (
        "01234567-89ab-cdef-aaaa-0000aaaaffff",
        UUID.unsafeFromString("01234567-89ab-cdef-aaaa-0000aaaaffff"),
      ),
    },
    ((str, exp)) => testParse(UUID.parser, str, exp, {pos: 36, str}),
  )

  test("parse/show round trip", () => {
    let uuid = "01234567-89ab-cdef-aaaa-0000aaaaffff"
    expect(UUID.parse(uuid)->(Relude.Result.map(UUID.show, _)))->toEqual(Relude.Result.ok(uuid))
  })

  testAll(
    "invalid",
    list{
      ("", 0),
      ("x", 0),
      ("00000000-1111-2222-3333-44444444444g", 35),
      ("00000000-1111-2222-3333-44444444444", 35),
      ("0000000-1111-2222-3333-444444444444", 7),
    },
    ((str, pos)) => testParseFail(UUID.parser, str, pos),
  )
})
