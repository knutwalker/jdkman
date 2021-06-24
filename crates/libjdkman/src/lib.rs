use once_cell::sync::OnceCell;

mod candidate;
mod commands;
mod sdkman;
mod select;

pub mod prelude {
    pub use crate::{
        candidate::Candidate,
        commands::results::*,
        commands::{JdkCurrent, JdkDefault, JdkInstall, JdkList, JdkUse},
        eprintln_color, eprintln_green, eprintln_red, eprintln_yellow,
        sdkman::{candidates_api, candidates_dir, platform},
    };
}

pub use select::fzf_command;

pub fn use_color() -> bool {
    fn check_for_colors() -> bool {
        let use_color = if clicolors_control::colors_enabled() {
            // check if colors are explicitly disabled
            sdkman::config()
                .iter()
                .all(|(k, v)| k != "sdkman_colour_enable" || v != "false")
        } else {
            // check if colors are explicitly enabled
            sdkman::config()
                .iter()
                .any(|(k, v)| k == "sdkman_colour_enable" && v == "true")
        };
        console::set_colors_enabled(use_color);
        use_color
    }

    static SDKMAN_COLOR_ENABLED: OnceCell<bool> = OnceCell::new();
    *SDKMAN_COLOR_ENABLED.get_or_init(check_for_colors)
}

#[macro_export]
macro_rules! eprint_color {
    ($color:path, $($arg:tt)*) => {
        $crate::eprint_color!(@ ::console::Style::new().fg($color), $($arg)*);
    };

    (@ $style:expr, $($arg:tt)*) => ({
        if $crate::use_color() {
            let text = ::std::fmt::format(format_args!($($arg)*));
            eprint!("{}", $style.apply_to(text));
        } else {
            eprint!($($arg)*);
        }
    })
}

#[macro_export]
macro_rules! eprintln_color {
    ($color:path, $($arg:tt)*) => {
        $crate::eprintln_color!(@ ::console::Style::new().fg($color), $($arg)*);
    };

    (@ $style:expr, $($arg:tt)*) => ({
        if $crate::use_color() {
            let text = ::std::fmt::format(format_args!($($arg)*));
            eprintln!("{}", $style.apply_to(text));
        } else {
            eprintln!($($arg)*);
        }
    })
}

#[macro_export]
macro_rules! eprintln_green {
    ($($arg:tt)*) => { $crate::eprintln_color!(::console::Color::Green, $($arg)*); }
}

#[macro_export]
macro_rules! eprintln_red {
    ($($arg:tt)*) => { $crate::eprintln_color!(::console::Color::Red, $($arg)*); }
}

#[macro_export]
macro_rules! eprintln_yellow {
    ($($arg:tt)*) => { $crate::eprintln_color!(::console::Color::Yellow, $($arg)*); }
}
