use rand::distr::Alphanumeric;
use rand::Rng;

pub fn random_namespace() -> String {
    let mut rng = rand::rng();
    (0..8)
        .map(|_| rng.sample(Alphanumeric) as char)
        .collect()
}
