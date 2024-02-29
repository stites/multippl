use crate::data::HashSet;
use std::str::FromStr;
use std::time::Duration;

macro_rules! zoom_and_enhance {
    (pub struct $name:ident { $(pub $fname:ident : $ftype:ty,)* }) => {
        #[derive(Debug, Clone, serde::Deserialize)]
        pub struct $name {
            $(pub $fname : $ftype),*
        }

        impl $name {
            pub fn header() -> &'static [&'static str] {
                static NAMES: &'static [&'static str] = &[$(stringify!($fname)),*];
                NAMES
            }
        //     fn write_row() -> Vec<String> {
        //         let r = vec![];
        //         $(
        //             if stringify!($fname) == "duration" {
        // r.pushformat!("{:.2}", self.determinism);
        //             } else {
        //               r.push(format!("{}", self.$fname));
        //             }
        // let c2 = format!("{:?}", self.comptype);
        // let c3 = format!("{:.2}", self.determinism);
        // let c4 = format!("{}", self.seed);
        // let c5 = format!("{}", self.ix);
        // let c6 = format!("{}", self.acceptsize);
        // let c7 = format!("{}", self.distsize);
        // let c8 = format!("{}", self.numsize);
        // let c9 = format!("{}", self.calls);
        // let c10 = format!("{}", self.duration.as_millis());
        // [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]
        //             )*
        //         static NAMES: &'static [&'static str] = &[$(stringify!($fname)),*];
        //         NAMES
        //     }
        }
    }
}

#[derive(Debug, Clone, Hash, Copy, serde::Deserialize, Eq, PartialEq)]
pub enum CompileType {
    Exact,
    Approx,
    OptApx,
}
impl CompileType {
    pub fn use_sampled(&self) -> bool {
        match self {
            CompileType::Exact => false,
            _ => true,
        }
    }
}
impl FromStr for CompileType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        use CompileType::*;
        let approx = HashSet::from(["approx", "sampling", "sample", "is"]);
        let opt = HashSet::from(["approx-opt", "sampling-opt", "sample-opt", "opt"]);
        if s == "exact" {
            Ok(Exact)
        } else if approx.contains(s) {
            Ok(Approx)
        } else if opt.contains(s) {
            Ok(OptApx)
        } else {
            Err(format!(
                "{} is not a valid string. Choose one of \"approx\" \"exact\" \"opt\"",
                s
            ))
        }
    }
}

zoom_and_enhance! {
    pub struct Row {
        pub gridsize: usize,
        pub comptype: CompileType,
        pub determinism: f64,
        pub seed: Option<u64>,
        pub ix: u64,
        pub acceptsize: usize,
        pub distsize: usize,
        pub numsize: usize,
        pub calls: usize,
        pub duration: Duration,
    }
}
#[derive(Debug, Clone, serde::Deserialize)]
pub struct DumbRow {
    pub gridsize: usize,
    pub comptype: CompileType,
    pub determinism: f64,
    pub seed: i64,
    pub ix: u64,
    pub acceptsize: usize,
    pub distsize: usize,
    pub numsize: usize,
    pub calls: usize,
    pub duration: usize, // milliseconds
}
impl Row {
    pub fn csv_array(&self) -> [String; 10] {
        let c1 = format!("{}", self.gridsize);
        let c2 = format!("{:?}", self.comptype);
        let c3 = format!("{:.2}", self.determinism);
        let c4 = format!("{}", self.seed.map(|x| x as i64).unwrap_or_else(|| -1));
        let c5 = format!("{}", self.ix);
        let c6 = format!("{}", self.acceptsize);
        let c7 = format!("{}", self.distsize);
        let c8 = format!("{}", self.numsize);
        let c9 = format!("{}", self.calls);
        let c10 = format!("{}", self.duration.as_micros());
        [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]
    }
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug, serde::Deserialize)]
pub enum Det {
    Zero,
    TwentyFivePer,
    FiftyPer,
    SeventyFivePer,
}
impl FromStr for Det {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        use Det::*;
        let prefixes = ["0.", ".", ""];
        let options_for = |num: usize| {
            prefixes
                .iter()
                .map(|pre| pre.to_string() + &(num).to_string())
                .collect::<HashSet<String>>()
        };
        if options_for(75).contains(s) {
            Ok(Det::SeventyFivePer)
        } else if options_for(0).contains(s) {
            Ok(Det::Zero)
        } else if options_for(5).contains(s) {
            Ok(Det::FiftyPer)
        } else if options_for(25).contains(s) {
            Ok(Det::TwentyFivePer)
        } else {
            Err(format!(
                "{} is not a valid string. Choose one of \"0.25\" \"0.5\" \"0.75\"",
                s
            ))
        }
    }
}
impl Det {
    pub fn from_f64(f: f64) -> Det {
        if f == 0.0 {
            Det::Zero
        } else if f == 0.25 {
            Det::TwentyFivePer
        } else if f == 0.5 {
            Det::FiftyPer
        } else if f == 0.75 {
            Det::SeventyFivePer
        } else {
            panic!("machine error?")
        }
    }
    pub fn to_f64(&self) -> f64 {
        match self {
            Det::Zero => 0.0,
            Det::TwentyFivePer => 0.25,
            Det::FiftyPer => 0.5,
            Det::SeventyFivePer => 0.75,
        }
    }
}
#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug, serde::Deserialize)]
pub struct SummaryKey {
    pub comptype: CompileType,
    pub gridsize: usize,
    pub determinism: Det,
}
impl SummaryKey {
    pub fn new(comptype: CompileType, gridsize: usize, determinism: Det) -> Self {
        SummaryKey {
            gridsize,
            comptype,
            determinism,
        }
    }
    pub fn to_header() -> String {
        format!("grid\tcomptype\tdet")
    }
    pub fn to_string(&self) -> String {
        format!(
            "{}x{} \t{:?}   \t{}",
            self.gridsize,
            self.gridsize,
            self.comptype,
            self.determinism.to_f64()
        )
    }
    pub fn from_data(d: &DumbRow) -> Self {
        SummaryKey {
            gridsize: d.gridsize,
            comptype: d.comptype,
            determinism: Det::from_f64(d.determinism),
        }
    }
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct SummaryData {
    pub duration: usize,
    pub acceptsize: usize,
    pub distsize: usize,
    pub numsize: usize,
    pub calls: usize,
    pub nsamples: usize,
}
impl SummaryData {
    pub fn from_data(d: &DumbRow) -> Self {
        Self {
            acceptsize: d.acceptsize,
            distsize: d.distsize,
            numsize: d.numsize,
            calls: d.calls,
            duration: d.duration,
            nsamples: 1,
        }
    }
    pub fn plus(&self, o: &Self) -> Self {
        Self {
            acceptsize: self.acceptsize + o.acceptsize,
            distsize: self.distsize + o.distsize,
            numsize: self.numsize + o.numsize,
            calls: self.calls + o.calls,
            duration: self.duration + o.duration,
            nsamples: self.nsamples + o.nsamples,
        }
    }
    pub fn to_header() -> String {
        format!("acceptsize\tdistsize\tnumsize     \tcalls    \tnsamples\tduration(micro)")
    }
    pub fn to_string(&self) -> String {
        // calls vary quite a bit, so we want to adjust the size of padding, if possible
        let cstr = format!("{}", self.calls);
        // aiming for ~10 columns
        let cpadding = if cstr.len() > 5 { "" } else { "      " };
        format!(
            "{}         \t{}         \t{}        \t{}{}\t{}       \t{}",
            self.acceptsize,
            self.distsize,
            self.numsize,
            self.calls,
            cpadding,
            self.nsamples,
            self.duration
        )
    }
    pub fn avg(&self) -> Self {
        let acceptsize = (self.acceptsize as f64 / self.nsamples as f64) as usize;
        let distsize = (self.distsize as f64 / self.nsamples as f64) as usize;
        let numsize = (self.numsize as f64 / self.nsamples as f64) as usize;
        let calls = (self.calls as f64 / self.nsamples as f64) as usize;
        let duration = (self.duration as f64 / self.nsamples as f64) as usize;
        Self {
            acceptsize,
            distsize,
            numsize,
            calls,
            duration,
            nsamples: self.nsamples,
        }
    }
}
