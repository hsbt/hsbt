pub fn hello() -> String {  
    "Hello, World!".to_string()
}   

#[cfg(test)]
mod test_main {
    use super::*;

    #[test]
    fn test_main() {
        assert_eq!("Hello, World!", hello());
    }
}
