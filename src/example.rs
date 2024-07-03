pub const SIMPLE_LANGUAGE: (&str, &str) = (
    "Grammar",
    "Grammar <- &(A !'b') 'a'* B EOF
A <- 'a' A 'b' / ()
B <- 'b' B 'c' / ()
EOF <- !.
",
);
