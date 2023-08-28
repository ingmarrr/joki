use clap::Parser;


#[derive(clap::Parser)]
#[command(author, version, about, long_about=None)]
pub struct App {
    #[command(subcommand)]
    cmd: Cmd
}

#[derive(clap::Subcommand, Clone)]
pub enum Cmd {
    Run { 
        #[arg(short, long)]
        file: String 
    },
    Build,
}


fn main() {
    let app = App::parse();

    match app.cmd {
        Cmd::Run{ file } => {
            let fc = std::fs::read_to_string(&file);

            if let Err(err) = fc {
                println!("Error reading file :: `{}`", err);
                return;
            }
            println!("Running :: {}", file);
        }
        Cmd::Build => println!("Building"),
    }
}
