use crate::token::Tok;

pub struct Tuple<const N: usize>([char; N]);

impl<const N: usize> Tuple<N> {
    pub fn new() -> Self {
        Self(['\0'; N])
    }

    pub fn set(&mut self, index: usize, value: char) {
        self.0[index] = value;
    }

    pub fn get(&self, index: usize) -> char {
        self.0[index]
    }

    pub fn get_tok(&self, index: usize) -> Option<Tok> {
        match self.0[index] {
            '\0' => None,
            c => Some(Tok::from(c)),
        }
    }

    pub fn get_str(&self) -> String {
        let mut s = String::new();
        for c in self.0.iter() {
            if *c == '\0' {
                break;
            }
            s.push(*c);
        }
        s
    }
}

impl Tuple<1> {
    pub fn splat(&self) -> (Option<Tok>,) {
        (self.get_tok(0),)
    }
}

impl Tuple<2> {
    pub fn splat(&self) -> (Option<Tok>, Option<Tok>) {
        (self.get_tok(0), self.get_tok(1))
    }
}

impl Tuple<3> {
    pub fn splat(&self) -> (Option<Tok>, Option<Tok>, Option<Tok>) {
        (self.get_tok(0), self.get_tok(1), self.get_tok(2))
    }
}

impl Tuple<4> {
    pub fn splat(&self) -> (Option<Tok>, Option<Tok>, Option<Tok>, Option<Tok>) {
        (
            self.get_tok(0),
            self.get_tok(1),
            self.get_tok(2),
            self.get_tok(3),
        )
    }
}

impl Tuple<5> {
    pub fn splat(
        &self,
    ) -> (
        Option<Tok>,
        Option<Tok>,
        Option<Tok>,
        Option<Tok>,
        Option<Tok>,
    ) {
        (
            self.get_tok(0),
            self.get_tok(1),
            self.get_tok(2),
            self.get_tok(3),
            self.get_tok(4),
        )
    }
}

impl Tuple<6> {
    pub fn splat(
        &self,
    ) -> (
        Option<Tok>,
        Option<Tok>,
        Option<Tok>,
        Option<Tok>,
        Option<Tok>,
        Option<Tok>,
    ) {
        (
            self.get_tok(0),
            self.get_tok(1),
            self.get_tok(2),
            self.get_tok(3),
            self.get_tok(4),
            self.get_tok(5),
        )
    }
}
