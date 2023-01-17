fn fill(vec: &mut Vec<Box<i32>>) {
    for i in 0..10 {
        vec.push(Box::new(i));
    }
}

fn print_contents(vec: &Vec<Box<i32>>) {
    for i in vec.into_iter() {
        print!("{} ", **i) //Note: `*i` and `i` also work here, because `print!` works for any format arguments whose types implement the `Display` trait.
    }
    println!();
}

fn swallow(_vec: Vec<Box<i32>>) {
    println!("Aaaand it's gone");
}

fn get_first(vec: &mut Vec<Box<i32>>, default: i32) -> i32 {
    // Why do we map here? Why do we need the two dereferences (**)?
    let first: Option<i32> = vec.first().map(|i| **i);
    match first {
        Some(x) => return x,
        None => {
            vec.push(Box::new(default));
            return default;
        }
    }
}

fn inc(i: &mut Box<i32>) {
    // Increment the i32 in the Box, i.e. the heap-allocated i32.
    **i += 1;
}

fn inc_all(vec: &mut Vec<Box<i32>>) {
    for i in vec.into_iter() {
        inc(i);
    }
}

fn main() {
    // Don't change this line:
    let mut vec: Vec<Box<i32>> = Vec::new();

    // You can change these lines:
    println!("First element: {}", get_first(&mut vec, -1));
    fill(&mut vec);
    print_contents(&vec);
    inc_all(&mut vec);
    print_contents(&vec);
    swallow(vec);
}


/*  ************ NOTE **************
Ownership Rules:
Each value in Rust has an owner.
There can only be one owner at a time.
When the owner goes out of scope, the value will be dropped.

Ownership and Functions
The mechanics of passing a value to a function are similar to those when assigning a value to a variable. 
Passing a variable to a function will move or copy, just as assignment does.
*/

fn main() {
    let s = String::from("hello");  // s comes into scope

    takes_ownership(s);             // s's value moves into the function...
                                    // ... and so is no longer valid here

    let x = 5;                      // x comes into scope

    makes_copy(x);                  // x would move into the function,
                                    // but i32 is Copy, so it's okay to still
                                    // use x afterward

} // Here, x goes out of scope, then s. But because s's value was moved, nothing
  // special happens.
fn takes_ownership(some_string: String) { // some_string comes into scope
    println!("{}", some_string);
} // Here, some_string goes out of scope and `drop` is called. The backing
  // memory is freed.
fn makes_copy(some_integer: i32) { // some_integer comes into scope
    println!("{}", some_integer);
} // Here, some_integer goes out of scope. Nothing special happens.

/*
Returning values can also transfer ownership.
*/
fn main() {
    let s1 = gives_ownership();         // gives_ownership moves its return
                                        // value into s1
    let s2 = String::from("hello");     // s2 comes into scope
    let s3 = takes_and_gives_back(s2);  // s2 is moved into
                                        // takes_and_gives_back, which also
                                        // moves its return value into s3
} // Here, s3 goes out of scope and is dropped. s2 was moved, so nothing
  // happens. s1 goes out of scope and is dropped.
fn gives_ownership() -> String {             // gives_ownership will move its
                                             // return value into the function
                                             // that calls it
    let some_string = String::from("yours"); // some_string comes into scope
    some_string                              // some_string is returned and
                                             // moves out to the calling
                                             // function
}
// This function takes a String and returns one
fn takes_and_gives_back(a_string: String) -> String { // a_string comes into
                                                      // scope
    a_string  // a_string is returned and moves out to the calling function
}

/*  (immutable) reference borrowing
the value pointed to by the reference is not dropped when s stops being used, 
because s doesn’t have ownership.When functions have references as parameters instead of the actual values, 
we won’t need to return the values in order to give back ownership, because we never had ownership.
*/
fn main() {
    let s1 = String::from("hello");
    let len = calculate_length(&s1);
    println!("The length of '{}' is {}.", s1, len);
}
fn calculate_length(s: &String) -> usize { // s is a reference to a String
    s.len()
} // Here, s goes out of scope. But because it does not have ownership of what
  // it refers to, it is not dropped.

/* mutable reference
Mutable references have one big restriction: if you have a mutable reference to a value,
you can have no other references to that value.
*/
fn main() {
    let mut s = String::from("hello");
    change(&mut s);
}
fn change(some_string: &mut String) {
    some_string.push_str(", world");
}
/* The Rules of References***
At any given time, you can have either one mutable reference or any number of immutable references.
References must always be valid. Do not return a string reference,return a string directly instead.
*/


