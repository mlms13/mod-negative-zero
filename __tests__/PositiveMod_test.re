open Jest;
open Expect;

let positiveMod = (a, b) => {
  let remainder = a mod b;
  remainder < 0 ? remainder + abs(b) : remainder;
};

let positiveMod' = (a, b) => {
  let remainder = a mod b;
  remainder < 0 ? abs(remainder + abs(b)) : remainder;
};

let positiveMod'' = (a, b) =>
  if (abs(a) == abs(b)) {
    0;
  } else {
    let remainder = a mod b;
    remainder < 0 ? abs(remainder + abs(b)) : remainder;
  };

let positiveMod''' = (a, b) => (a mod b + b) mod b;

let positiveMod'''' = (a, b) => (a mod b + abs(b)) mod b;

let makeTests = modulo => {
  test("dividend and divisor both positive", () =>
    expect(modulo(4, 3)) |> toEqual(1)
  );

  test("dividend negative", () =>
    expect(modulo(-4, 3)) |> toEqual(2)
  );

  test("divisor negative", () =>
    expect(modulo(4, -3)) |> toEqual(1)
  );

  test("dividend negative, no remainder", () =>
    expect(modulo(-4, 4)) |> toEqual(0)
  );
};

describe("positiveMod (one mod, no extra abs)", () =>
  makeTests(positiveMod)
);

describe("positiveMod (one mod, abs when remainder is negative)", () =>
  makeTests(positiveMod')
);

describe("positiveMod (one mod, special case 0 before mod)", () =>
  makeTests(positiveMod'')
);

describe("positiveMod (two mods)", () =>
  makeTests(positiveMod''')
);

describe("positiveMod (two mods, abs divisor)", () =>
  makeTests(positiveMod'''')
);
