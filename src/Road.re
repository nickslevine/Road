module String = {
  let reverse = str => {
    let len = String.length(str);
    let reversed = String.init(len, i => str.[len - 1 - i]);
    reversed;
  };
};

module List = {
  let max = lst => {
    List.fold_left(
      (acc, x) =>
        if (acc > x) {
          acc;
        } else {
          x;
        },
      neg_infinity,
      lst,
    );
  };

  let min = lst => {
    List.fold_left(
      (acc, x) =>
        if (acc < x) {
          acc;
        } else {
          x;
        },
      infinity,
      lst,
    );
  };

  let sum = lst => {
    List.fold_left((acc, x) => acc + x, 0, lst);
  };

  let product = lst => {
    List.fold_left((acc, x) => acc * x, 1, lst);
  };

  let rec range = (start: int, end_: int) =>
    if (start >= end_) {
      [];
    } else {
      [start, ...range(start + 1, end_)];
    };
};