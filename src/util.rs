use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::str::FromStr;


/// Helper function to cross product a list of values `ts` across `n` variables.
pub fn self_product<T: Clone>(ts: &[T], n: usize) -> Vec<Vec<T>> {
    (0..n)
        .map(|i| {
            let mut res = vec![];
            let nc = ts.len();
            let nrows = nc.pow(n as u32);
            while res.len() < nrows {
                for c in ts {
                    for _ in 0..nc.pow(i as u32) {
                        res.push(c.clone())
                    }
                }
            }
            res
        })
        .collect()
}

// Division the rounds up
// Hack from https://www.reddit.com/r/rust/comments/bk7v15/my_next_favourite_way_to_divide_integers_rounding/
pub fn div_up(a : usize, b: usize) -> usize {
    (0..a).step_by(b).size_hint().0
}

// read cvec values from a file
// TODO: this seems like an inelegant imposition, probably rewrite later 
pub fn read_cvec_from_file<T: FromStr>(filename: String) -> Vec<T> where <T as FromStr>::Err: std::fmt::Debug, T: std::fmt::Debug {
// pub fn read_cvec_from_file<T: FromStr>(filename: String) -> Vec<T> {
    // let contents = fs::read_to_string(filename)
    //     .expect("Something went wrong reading the file");
        
    // println!("With text:\n{}", contents);   

    let mut consts: Vec<T> = vec![];

    if let Ok(lines) = read_lines(filename) {
        // Consumes the iterator, returns an (Optional) String
        for line in lines {
            if let Ok(ip) = line {
                let result : T = ip.parse().unwrap(); 
                consts.push(result)
            }
        }
    }

    consts
} 

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_product() {
        let ts = &[4, 5, 6];
        assert_eq!(
            self_product(ts, 2),
            vec![
                vec![4, 5, 6, 4, 5, 6, 4, 5, 6],
                vec![4, 4, 4, 5, 5, 5, 6, 6, 6]
            ],
        );
    }
}

