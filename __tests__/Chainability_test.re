open Jest;
open Chalk;
open Expect;

describe("Chainability", () => {
  test("Color and underline", () => {
    let t = "You Know Nothing Jon Snow";
    let x = t |> colorify(RedBright) |> underline;
    x |> Js.log;
    x |> expect |> toMatch(t)
  });

  test("Color and underline, bold", () => {
    let t = "You Know Nothing Jon Snow";
    let x = t |> colorify(RedBright) |> underline |> bold;
    x |> Js.log;
    x |> expect |> toMatch(t)
  });

  test("Color and underline, bold, inverse", () => {
    let t = "You Know Nothing Jon Snow";
    let x = t |> colorify(RedBright) |> underline |> bold |> inverse;
    x |> Js.log;
    x |> expect |> toMatch(t)
  });

  test("Color and underline, bold, background", () => {
    let t = "You Know Nothing Jon Snow";
    let x = t |> colorify(RedBright) |> underline |> bold |> bgWhite;
    x |> Js.log;
    x |> expect |> toMatch(t)
  });

  test("Color and underline, bold, background, inverse", () => {
    let t = "You Know Nothing Jon Snow";
    let x = t |> colorify(RedBright) |> underline |> bold |> bgWhite |> inverse;
    x |> Js.log;
    x |> expect |> toMatch(t)
  });
});