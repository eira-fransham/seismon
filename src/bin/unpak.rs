use std::{
    fs,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
    process::exit,
};

use bevy_mod_pakfile::Pak;

use clap::Parser;

#[expect(dead_code)]
#[derive(Debug, Parser)]
struct Opt {
    #[arg(short, long)]
    verbose: bool,

    #[arg(long)]
    version: bool,

    #[arg(name = "INPUT_PAK")]
    input_pak: PathBuf,

    #[arg(name = "OUTPUT_DIR")]
    output_dir: Option<PathBuf>,
}

const VERSION: &str = "
unpak 0.1
Copyright © 2020 Cormac O'Brien
Released under the terms of the MIT License
";

fn main() {
    let opt = Opt::parse();

    if opt.version {
        println!("{VERSION}");
        exit(0);
    }

    let pak = match Pak::open(&opt.input_pak) {
        Ok(p) => p,
        Err(why) => {
            println!("Couldn't open {:#?}: {}", opt.input_pak, why);
            exit(1);
        }
    };

    for (k, v) in pak.iter() {
        let mut path = PathBuf::new();

        if let Some(ref d) = opt.output_dir {
            path.push(d);
        }

        path.push(k);

        if let Some(p) = path.parent()
            && !p.exists()
            && let Err(why) = fs::create_dir_all(p)
        {
            println!("Couldn't create parent directories: {why}");
            exit(1);
        }

        let file = match File::create(&path) {
            Ok(f) => f,
            Err(why) => {
                println!("Couldn't open {}: {}", path.to_str().unwrap(), why);
                exit(1);
            }
        };

        let mut writer = BufWriter::new(file);
        match writer.write_all(v.as_ref()) {
            Ok(_) => (),
            Err(why) => {
                println!("Couldn't write to {}: {}", path.to_str().unwrap(), why);
                exit(1);
            }
        }
    }
}
