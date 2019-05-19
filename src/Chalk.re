type t;
type foo = string => string;
[@bs.module "chalk"] external reset: foo = "";
[@bs.module "chalk"] external bold: foo = "";
[@bs.module "chalk"] external dim: foo = "";
[@bs.module "chalk"] external italic: foo = "";
[@bs.module "chalk"] external underline: foo = "";
[@bs.module "chalk"] external inverse: foo = "";
[@bs.module "chalk"] external hidden: foo = "";
[@bs.module "chalk"] external strikethrough: foo = "";
[@bs.module "chalk"] external visible: foo = "";
[@bs.module "chalk"] external black: foo = "";
[@bs.module "chalk"] external red: foo = "";
[@bs.module "chalk"] external green: foo = "";
[@bs.module "chalk"] external yellow: foo = "";
[@bs.module "chalk"] external blue: foo = "";
[@bs.module "chalk"] external magenta: foo = "";
[@bs.module "chalk"] external cyan: foo = "";
[@bs.module "chalk"] external white: foo = "";
[@bs.module "chalk"] external gray: foo = "";
[@bs.module "chalk"] external redBright: foo = "";
[@bs.module "chalk"] external greenBright: foo = "";
[@bs.module "chalk"] external yellowBright: foo = "";
[@bs.module "chalk"] external blueBright: foo = "";
[@bs.module "chalk"] external magentaBright: foo = "";
[@bs.module "chalk"] external cyanBright: foo = "";
[@bs.module "chalk"] external whiteBright: foo = "";
[@bs.module "chalk"] external bgBlack: foo = "";
[@bs.module "chalk"] external bgRed: foo = "";
[@bs.module "chalk"] external bgGreen: foo = "";
[@bs.module "chalk"] external bgYellow: foo = "";
[@bs.module "chalk"] external bgBlue: foo = "";
[@bs.module "chalk"] external bgMagenta: foo = "";
[@bs.module "chalk"] external bgCyan: foo = "";
[@bs.module "chalk"] external bgWhite: foo = "";
[@bs.module "chalk"] external bgBlackBright: foo = "";
[@bs.module "chalk"] external bgRedBright: foo = "";
[@bs.module "chalk"] external bgGreenBright: foo = "";
[@bs.module "chalk"] external bgYellowBright: foo = "";
[@bs.module "chalk"] external bgBlueBright: foo = "";
[@bs.module "chalk"] external bgMagentaBright: foo = "";
[@bs.module "chalk"] external bgCyanBright: foo = "";
[@bs.module "chalk"] external bgWhiteBright: foo = "";


type rgbf = (~r: int, ~g: int, ~b:int) => foo;
[@bs.module "chalk"] external rgb: rgbf = "";
[@bs.module "chalk"] external bgRgb: rgbf = "";

[@bs.module "chalk"] external hex: foo = "";
[@bs.module "chalk"] external bgHex: foo = "";

type hslf = (~h: int, ~s: int, ~l: int) => foo;
[@bs.module "chalk"] external hsl: hslf = "";
[@bs.module "chalk"] external bgHsl: hslf = "";

type hsvf = (~h: int, ~s: int, ~v: int) => foo;
[@bs.module "chalk"] external hsv: hsvf = "";
[@bs.module "chalk"] external bgHsv: hsvf = "";

type hwbf = (~h: int, ~w: int, ~b: int) => foo;
[@bs.module "chalk"] external hwb: hwbf = "";
[@bs.module "chalk"] external bgHwb: hwbf = "";

type keywordf = string => string;
[@bs.module "chalk"] external keyword: keywordf = "";
[@bs.module "chalk"] external bgKeyword: keywordf = "";


type color =
  | Reset
  | Bold
  | Dim
  | Italic
  | Underline
  | Inverse
  | Hidden
  | Strikethrough
  | Visible
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Gray
  | RedBright
  | GreenBright
  | YellowBright
  | BlueBright
  | MagentaBright
  | CyanBright
  | WhiteBright
  | BgBlack
  | BgRed
  | BgGreen
  | BgYellow
  | BgBlue
  | BgMagenta
  | BgCyan
  | BgWhite
  | BgBlackBright
  | BgRedBright
  | BgGreenBright
  | BgYellowBright
  | BgBlueBright
  | BgMagentaBright
  | BgCyanBright
  | BgWhiteBright
  // | RGB(int, int, int)
  // | HSL(int, int, int)
  // | Hex(string)
  // | HSV(int, int, int)
  // | HWB(int, int, int)
;

let colorify = (color: color, text: string): string =>
  switch (color) {
  | Reset => reset(text)
  | Bold => bold(text)
  | Dim => dim(text)
  | Italic => italic(text)
  | Underline => underline(text)
  | Inverse => inverse(text)
  | Hidden => hidden(text)
  | Strikethrough => strikethrough(text)
  | Visible => visible(text)
  | Black => black(text)
  | Red => red(text)
  | Green => green(text)
  | Yellow => yellow(text)
  | Blue => blue(text)
  | Magenta => magenta(text)
  | Cyan => cyan(text)
  | White => white(text)
  | Gray => gray(text)
  | RedBright => redBright(text)
  | GreenBright => greenBright(text)
  | YellowBright => yellowBright(text)
  | BlueBright => blueBright(text)
  | MagentaBright => magentaBright(text)
  | CyanBright => cyanBright(text)
  | WhiteBright => whiteBright(text)
  | BgBlack => bgBlack(text)
  | BgRed => bgRed(text)
  | BgGreen => bgGreen(text)
  | BgYellow => bgYellow(text)
  | BgBlue => bgBlue(text)
  | BgMagenta => bgMagenta(text)
  | BgCyan => bgCyan(text)
  | BgWhite => bgWhite(text)
  | BgBlackBright => bgBlackBright(text)
  | BgRedBright => bgRedBright(text)
  | BgGreenBright => bgGreenBright(text)
  | BgYellowBright => bgYellowBright(text)
  | BgBlueBright => bgBlueBright(text)
  | BgMagentaBright => bgMagentaBright(text)
  | BgCyanBright => bgCyanBright(text)
  | BgWhiteBright => bgWhiteBright(text)
  // | RGB(r, g, b) => rgb(~r, ~g, ~b)(text)
  // | HSL(h, s, l) => hsl(~h, ~s, ~l)(text)
  // | Hex(h) => hex(h)
  // | HSV(h, s, v) => hsv(~h, ~s, ~v)(text)
  // | HWB(h, w, b) => hwb(~h, ~w, ~b)(text)
  };

module Helpers = {
  let stringToColor = (t: string): color =>
    switch (t) {
    | "reset" => Reset
    | "bold" => Bold
    | "dim" => Dim
    | "italic" => Italic
    | "underline" => Underline
    | "inverse" => Inverse
    | "hidden" => Hidden
    | "strikethrough" => Strikethrough
    | "visible" => Visible
    | "black" => Black
    | "red" => Red
    | "green" => Green
    | "yellow" => Yellow
    | "blue" => Blue
    | "magenta" => Magenta
    | "cyan" => Cyan
    | "white" => White
    | "gray" => Gray
    | "redBright" => RedBright
    | "greenBright" => GreenBright
    | "yellowBright" => YellowBright
    | "blueBright" => BlueBright
    | "magentaBright" => MagentaBright
    | "cyanBright" => CyanBright
    | "whiteBright" => WhiteBright
    | "bgBlack" => BgBlack
    | "bgRed" => BgRed
    | "bgGreen" => BgGreen
    | "bgYellow" => BgYellow
    | "bgBlue" => BgBlue
    | "bgMagenta" => BgMagenta
    | "bgCyan" => BgCyan
    | "bgWhite" => BgWhite
    | "bgBlackBright" => BgBlackBright
    | "bgRedBright" => BgRedBright
    | "bgGreenBright" => BgGreenBright
    | "bgYellowBright" => BgYellowBright
    | "bgBlueBright" => BgBlueBright
    | "bgMagentaBright" => BgMagentaBright
    | "bgCyanBright" => BgCyanBright
    | "bgWhiteBright" => BgWhiteBright
    | _ => Reset
    };

  let colorToString = (c: color) =>
    switch (c) {
    | Reset => "reset"
    | Bold => "bold"
    | Dim => "dim"
    | Italic => "italic"
    | Underline => "underline"
    | Inverse => "inverse"
    | Hidden => "hidden"
    | Strikethrough => "strikethrough"
    | Visible => "visible"
    | Black => "black"
    | Red => "red"
    | Green => "green"
    | Yellow => "yellow"
    | Blue => "blue"
    | Magenta => "magenta"
    | Cyan => "cyan"
    | White => "white"
    | Gray => "gray"
    | RedBright => "redBright"
    | GreenBright => "greenBright"
    | YellowBright => "yellowBright"
    | BlueBright => "blueBright"
    | MagentaBright => "magentaBright"
    | CyanBright => "cyanBright"
    | WhiteBright => "whiteBright"
    | BgBlack => "bgBlack"
    | BgRed => "bgRed"
    | BgGreen => "bgGreen"
    | BgYellow => "bgYellow"
    | BgBlue => "bgBlue"
    | BgMagenta => "bgMagenta"
    | BgCyan => "bgCyan"
    | BgWhite => "bgWhite"
    | BgBlackBright => "bgBlackBright"
    | BgRedBright => "bgRedBright"
    | BgGreenBright => "bgGreenBright"
    | BgYellowBright => "bgYellowBright"
    | BgBlueBright => "bgBlueBright"
    | BgMagentaBright => "bgMagentaBright"
    | BgCyanBright => "bgCyanBright"
    | BgWhiteBright => "bgWhiteBright"
    // | RGB(r, g, b) => "rgb("++string_of_int(r)++","++string_of_int(g)++","++string_of_int(b)++")"
    // | HSL(h, s, l) => "hsl("++string_of_int(h)++","++string_of_int(s)++","++string_of_int(l)++")"
    // | Hex(h) => h
    // | HSV(h, s, v) => "hsv("++string_of_int(h)++","++string_of_int(s)++","++string_of_int(v)++")"
    // | HWB(h, w, b) => "hwb("++string_of_int(h)++","++string_of_int(w)++","++string_of_int(b)++")"
    };
};