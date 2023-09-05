// Make a macro for logging, that takes in arguments, passes them into tracing, but also
// adds some default data to the log message.
// For example I want to add either "LEX" (Lexing), "PAR" (Parsing), "LOA" (Lookahead)
#[macro_export]
macro_rules! log {
    (LEX, $level:expr, $($arg:tt)+) => {
        tracing::event!($level, "LEXING  :: {}", format_args!($($arg)+));
    };
    (PAR, $level:expr, $($arg:tt)+) => {
        tracing::event!($level, "PARSING :: {}", format_args!($($arg)+));
    };
    (LA, $level:expr, $($arg:tt)+) => {
        let tmp = format!("LOA :: {}", format_args!($($arg)+));
        tracing::event!($level, "PEEKED :: {}", format_args!($($arg)+));
    };
    ($level:expr, $($arg:tt)+) => {
        let tmp = format!("    :: {}", format_args!($($arg)+));
        tracing::event!($level, "{}", format_args!($($arg)+));
    };
}

#[macro_export]
macro_rules! info {
    (LEX, $($arg:tt)+) => {
        log!(LEX, tracing::Level::INFO, $($arg)+);
    };
    (PAR, $($arg:tt)+) => {
        log!(PAR, tracing::Level::INFO, $($arg)+);
    };
    (LA, $($arg:tt)+) => {
        log!(LA, tracing::Level::INFO, $($arg)+);
    };
    ($($arg:tt)+) => {
        log!(tracing::Level::INFO, $($arg)+);
    };
}

#[macro_export]
macro_rules! warn {
    (LEX, $($arg:tt)+) => {
        log!(LEX, tracing::Level::WARN, $($arg)+);
    };
    (PAR, $($arg:tt)+) => {
        log!(PAR, tracing::Level::WARN, $($arg)+);
    };
    (LA, $($arg:tt)+) => {
        log!(LA, tracing::Level::WARN, $($arg)+);
    };
    ($($arg:tt)+) => {
        log!(tracing::Level::WARN, $($arg)+);
    };
}

#[macro_export]
macro_rules! error {
    (LEX, $($arg:tt)+) => {
        log!(LEX, tracing::Level::ERROR, $($arg)+);
    };
    (PAR, $($arg:tt)+) => {
        log!(PAR, tracing::Level::ERROR, $($arg)+);
    };
    (LA, $($arg:tt)+) => {
        log!(LA, tracing::Level::ERROR, $($arg)+);
    };
    ($($arg:tt)+) => {
        log!(tracing::Level::ERROR, $($arg)+);
    };
}
