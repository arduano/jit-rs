pub struct Counter {
    count: u32,
}

impl Counter {
    pub fn new() -> Self {
        Counter { count: 0 }
    }

    pub fn next(&mut self) -> u32 {
        self.count += 1;
        self.count
    }
}
