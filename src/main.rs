use clap::Parser;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(clap::Parser)]
#[command(author, version, about, long_about=None)]
pub struct App {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(clap::Subcommand, Clone)]
pub enum Cmd {
    Run {
        #[arg(short, long)]
        file: String,
    },
    Build,
}

fn main() {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    let app = App::parse();

    match app.cmd {
        Cmd::Run { file } => {
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
