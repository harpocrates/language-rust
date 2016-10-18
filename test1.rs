
// This is a comment, and will be ignored by the compiler
// You can test this code by clicking the "Run" button over there ->
// or if prefer to use your keyboard, you can use the "Ctrl + Enter" shortcut

// This code is editable, feel free to hack it!
// You can always return to the original code by clicking the "Reset" button ->

// This is the main function
fn main() {
    // The statements here will be executed when the compiled binary is called

    // Print text to the console
    println!("Hello World!");

	/* Block comments which go to the closing delimiter. */


    // Create a structure which contains an `i32`. Name it `Structure`.
    #[allow(dead_code)]
    #[derive(Debug)]
    struct Structure { f: i32 };

    // In general, the `{}` will be automatically replaced with any
    // arguments. These will be stringified.
    println!("{} days", 31);

    // `Structure` is printable!
    println!("Now {:?} will print!", Structure { f: 3, ..(Structure { f: 1 } ) });
    let t: (i32,) = (1,);

    let e = 
    unsafe {
    	1+2;
    	2+3
    };

    let x1 = [1,200,3];
    let x2 = [1,200,3,];
    let x3 = [1;200];
    let x4 = [1;200];

    let mut n = 3;
    n + = 2;
}
