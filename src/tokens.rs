struct Ident {
    pub name: String,
}

enum Token {
    Fn,
    Ident(Ident),
}
