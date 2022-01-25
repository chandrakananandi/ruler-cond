use std::fs::File;
use std::io::Write; 
use std::io::BufReader;
use std::path::Path;
use std::fs::OpenOptions;
use std::io::{self, BufRead};
use std::str::FromStr;
use serde::Serialize;
// use serde::DeserializeOwned;
use serde::de;
use serde::Deserializer;


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

// impl<'de> Deserialize<'de> for T where T: FromStr {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//         where D: Deserializer<'de>
//     {
//         let s = String::deserialize(deserializer)?;
//         FromStr::from_str(&s).map_err(de::Error::custom)
//     }
// }

/*
pub fn read_ces_from_file<'a, T: DeserializeOwned<'a>>(filename: String) -> Vec<T> {
    let file = File::open(Path::new(&filename)).unwrap();
    let reader = BufReader::new(file);

    let ces : Vec<T> = serde_json::from_reader(reader).unwrap();
    

    ces

} */

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

// TODO: change this slightly
pub fn write_cvec_to_file<T: Serialize>(filename: String, consts: Vec<T>) {
// pub fn write_cvec_to_file<T: FromStr + std::fmt::Display>(filename: String, consts: Vec<T>) {
    // pub fn read_cvec_from_file<T: FromStr>(filename: String) -> Vec<T> {
        // let contents = fs::read_to_string(filename)
        //     .expect("Something went wrong reading the file");
            
        // println!("With text:\n{}", contents);   
        
        let mut f = OpenOptions::new()
        .write(true)
        // .append(true) // normally true but with the array... we can't simply append anymore
        // we could format each cvec manually and write line-by-line though... might  be better
        .create(true)
        .open(filename)
        .unwrap();

        let s = serde_json::to_string(&consts).unwrap();
        write!(f, "{}", s);

        // poor man's json
        // writeln!(f, "[");

        // // let mut f = File::create(filename).expect("Unable to create file");                                                                                                          
        // for i in &consts{                                                                                                                                                                  
        //     writeln!(f, "{},", i).expect("Unable to write to file");
        // }

        // writeln!(f, "]");
    
    
        
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

