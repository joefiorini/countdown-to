  /* Borrowed from: http://mihamina.rktmb.org/2013/02/ocaml-random-string-and-word-generation.html */

  let rand_chr = () => Char.chr(97 + Random.int(26));

let rec rand_voy = () => {
  let got = rand_chr();
  switch (got) {
  | 'a'
  | 'e'
  | 'i'
  | 'o'
  | 'u'
  | 'y' => got
  | _ => rand_voy()
  };
};

let rec rand_con = () => {
  let got = rand_chr();
  switch (got) {
  | 'a'
  | 'e'
  | 'i'
  | 'o'
  | 'u'
  | 'y' => rand_con()
  | _ => got
  };
};

let rec rand_convoy = (acc, syll_number, ()) =>
  switch (syll_number) {
  | 0 => acc
  | _ =>
    rand_convoy(
      acc ++ Char.escaped(rand_con()) ++ Char.escaped(rand_voy()),
      syll_number - 1,
      ()
    )
  };

  let get = () => rand_convoy("", 3, ());
