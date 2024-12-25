use anyhow::Result;
use rand::{
    distr::{Alphanumeric, Distribution, Uniform},
    Rng,
};

pub fn random_namespace() -> Result<String> {
    let mut rng = rand::rng();
    let first_char = Uniform::new_inclusive('a', 'z')?.sample(&mut rng) as char;
    let rest: String = (0..7).map(|_| rng.sample(Alphanumeric) as char).collect();
    Ok(format!("{}{}", first_char, rest))
}
