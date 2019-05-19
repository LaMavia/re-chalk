open Jest;
open Expect;

open Chalk;
let text = "Hello";
let colors =
  [|
    Reset,
    Bold,
    Dim,
    Italic,
    Underline,
    Inverse,
    Hidden,
    Strikethrough,
    Visible,
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Gray,
    RedBright,
    GreenBright,
    YellowBright,
    BlueBright,
    MagentaBright,
    CyanBright,
    WhiteBright,
    BgBlack,
    BgRed,
    BgGreen,
    BgYellow,
    BgBlue,
    BgMagenta,
    BgCyan,
    BgWhite,
    BgBlackBright,
    BgRedBright,
    BgGreenBright,
    BgYellowBright,
    BgBlueBright,
    BgMagentaBright,
    BgCyanBright,
    BgWhiteBright,
    // RGB(21, 21, 21),
    // HSL(21, 21, 21),
    // HSV(32, 100, 100),
    // HWB(32, 0, 50),
    // Hex("#ffffff"),
  |]
  |> Array.map(c => (c |> Chalk.Helpers.colorToString, c));

describe("Colors", () =>
  colors
  |> Array.iter(item => {
       let (t, c) = item;
       let x = Chalk.colorify(c, t);
       Js.log(x);
       test(t, () =>
         x |> expect |> toMatch(t)
       );
     })
);