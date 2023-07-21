use include_flate::*;
use rsgm::bayesian_network::BayesianNetwork;
use std::slice::Iter;
use std::str::FromStr;
use strum::ParseError;
use strum_macros::Display;
use strum_macros::EnumString;

flate!(pub static ALARM: str from "networks/alarm.json");
flate!(pub static ANDES: str from "networks/andes.json");
flate!(pub static BARLEY: str from "networks/barley.json");
flate!(pub static CANCER: str from "networks/cancer.json");
flate!(pub static DIABETES: str from "networks/diabetes.json");
flate!(pub static HAILFINDER: str from "networks/hailfinder.json");
flate!(pub static HEPAR2: str from "networks/hepar2.json");
flate!(pub static INSURANCE: str from "networks/insurance.json");
flate!(pub static LINK: str from "networks/link.json");
flate!(pub static MILDEW: str from "networks/mildew.json");
flate!(pub static PATHFINDER: str from "networks/pathfinder.json");
flate!(pub static SACHS: str from "networks/sachs.json");
flate!(pub static TINY: str from "networks/tiny.json");
flate!(pub static TINY2: str from "networks/tiny_2.json");
flate!(pub static WATER: str from "networks/water.json");
flate!(pub static WIN95PTS: str from "networks/win95pts.json");

#[derive(Debug, PartialEq, Clone, EnumString, Display)]
pub enum Specification {
    #[strum(ascii_case_insensitive)]
    Alarm,
    #[strum(ascii_case_insensitive)]
    Andes,
    #[strum(ascii_case_insensitive)]
    Barley,
    #[strum(ascii_case_insensitive)]
    Cancer,
    #[strum(ascii_case_insensitive)]
    Diabetes,
    #[strum(ascii_case_insensitive)]
    Hailfinder,
    #[strum(ascii_case_insensitive)]
    Hepar2,
    #[strum(ascii_case_insensitive)]
    Insurance,
    #[strum(ascii_case_insensitive)]
    Link,
    #[strum(ascii_case_insensitive)]
    Mildew,
    #[strum(ascii_case_insensitive)]
    Pathfinder,
    #[strum(ascii_case_insensitive)]
    Sachs,
    #[strum(ascii_case_insensitive)]
    Tiny,
    #[strum(ascii_case_insensitive)]
    Tiny2,
    #[strum(ascii_case_insensitive)]
    Water,
    #[strum(ascii_case_insensitive)]
    Win95pts,
}

impl Specification {
    pub fn iterator() -> Iter<'static, Specification> {
        use Specification::*;
        // NOTE: this is hand-sorted by variable size (smallest-to-largest)
        static SPECIFICATIONS: [Specification; 16] = [
            Tiny, Tiny2, Cancer, Sachs, Insurance, Water, Mildew, Alarm, Barley, Hailfinder,
            Hepar2, Win95pts, Pathfinder, Andes, Diabetes, Link,
        ];
        SPECIFICATIONS.iter()
    }

    // from_str with better errors
    pub fn _from_str(s: &str) -> Result<Specification, String> {
        match Specification::from_str(s) {
            Ok(s) => Ok(s),
            Err(ParseError::VariantNotFound) => Err(format!(
                "Got: {}, Expected one of: {}",
                s,
                Specification::iterator()
                    .map(|s| format!("{}", s).to_lowercase())
                    .collect::<Vec<String>>()
                    .join(", ")
            )),
        }
    }
    pub fn json(&self) -> &str {
        use Specification::*;
        match self {
            Alarm => &ALARM,
            Andes => &ANDES,
            Barley => &BARLEY,
            Cancer => &CANCER,
            Diabetes => &DIABETES,
            Hailfinder => &HAILFINDER,
            Hepar2 => &HEPAR2,
            Insurance => &INSURANCE,
            Link => &LINK,
            Mildew => &MILDEW,
            Pathfinder => &PATHFINDER,
            Sachs => &SACHS,
            Tiny => &TINY,
            Tiny2 => &TINY2,
            Water => &WATER,
            Win95pts => &WIN95PTS,
        }
    }
    pub fn network(&self) -> BayesianNetwork {
        BayesianNetwork::from_json(self.json())
    }

    pub fn nodes(&self) -> usize {
        use Specification::*;
        match self {
            Tiny => 1,
            Tiny2 => 5,
            Cancer => 5,
            Sachs => 11,
            Insurance => 27,
            Water => 32,
            Mildew => 35,
            Alarm => 37,
            Barley => 48,
            Hailfinder => 56,
            Hepar2 => 70,
            Win95pts => 76,
            Pathfinder => 109,
            Andes => 223,
            Diabetes => 413,
            Link => 724,
        }
    }

    // taken from https://www.bnlearn.com/bnrepository, although we can just compute this...
    pub fn arcs(&self) -> usize {
        use Specification::*;
        match self {
            Tiny => todo!(),
            Tiny2 => todo!(),
            Cancer => 4,
            Sachs => 17,
            Insurance => 52,
            Water => 66,
            Mildew => 46,
            Alarm => 46,
            Barley => 84,
            Hailfinder => 66,
            Hepar2 => 123,
            Win95pts => 112,
            Pathfinder => 195,
            Andes => 338,
            Diabetes => 602,
            Link => 1125,
        }
    }

    // taken from https://www.bnlearn.com/bnrepository, although we can just compute this...
    pub fn parameters(&self) -> usize {
        use Specification::*;
        match self {
            Tiny => todo!(),
            Tiny2 => todo!(),
            Cancer => 10,
            Sachs => 178,
            Insurance => 1008,
            Water => 10083,
            Mildew => 540150,
            Alarm => 509,
            Barley => 114005,
            Hailfinder => 2656,
            Hepar2 => 1453,
            Win95pts => 574,
            Pathfinder => 72079,
            Andes => 1157,
            Diabetes => 429409,
            Link => 14211,
        }
    }
    /// networks with < 20 nodes
    pub fn iterator_small() -> Iter<'static, Specification> {
        use Specification::*;
        static SPECIFICATIONS: [Specification; 4] = [Tiny, Tiny2, Cancer, Sachs];

        SPECIFICATIONS.iter()
    }
    /// networks with 20-50 nodes
    pub fn iterator_medium() -> Iter<'static, Specification> {
        use Specification::*;
        static SPECIFICATIONS: [Specification; 5] = [Insurance, Water, Mildew, Alarm, Barley];
        SPECIFICATIONS.iter()
    }
    /// networks with 50-100 nodes
    pub fn iterator_large() -> Iter<'static, Specification> {
        use Specification::*;
        static SPECIFICATIONS: [Specification; 3] = [Hailfinder, Hepar2, Win95pts];
        SPECIFICATIONS.iter()
    }
    /// networks with 100-1000 nodes
    pub fn iterator_vlarge() -> Iter<'static, Specification> {
        use Specification::*;
        static SPECIFICATIONS: [Specification; 4] = [Pathfinder, Andes, Diabetes, Link];
        SPECIFICATIONS.iter()
    }

    // TODO: treewidth, k-connectivity, structural hamming distance (https://sites.pitt.edu/~druzdzel/ftp/iis09.pdf)
    // TODO: massive networks as defined by https://www.bnlearn.com/bnrepository/
}
