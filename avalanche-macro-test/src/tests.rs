use avalanche::{component, reactive_assert, enclose, View};
#[derive(Default)]
struct HasFields {
    field_one: u8,
    field_two: u8
}

#[component]
fn Bare() -> View {
    ().into()
}

#[component]
fn Identity(a: u8) -> View {
    reactive_assert!(a => a);
    let a = a;
    reactive_assert!(a => a);

    ().into()
}

#[component]
fn ArrayIndex(a: u8, b: u8, c: u8) -> View {
    let arr = [a, b, c];
    reactive_assert!(a, b, c => arr);

    {
        let first = arr[0];
        let second = arr[1];
        let third = arr[2];
        reactive_assert!(a => first; b => second; c => third);
    }

    let arr2 = [a; 3];
    reactive_assert!(a => arr2);
    let from2 = arr2[0];
    reactive_assert!(a => from2);


    let indexed = arr2[*c as usize];
    reactive_assert!(c => indexed);

    ().into()
}

#[component]
fn Assign(a: u8, b: u8) -> View {
    let mut x = *a;
    reactive_assert!(a => x);
    x = *b;
    reactive_assert!(b => x);

    x = *a;
    reactive_assert!(a => x);

    x += *b;
    reactive_assert!(a, b => x);

    let mut has_fields = HasFields::default();

    has_fields.field_one = *a;
    reactive_assert!(a => has_fields);

    ().into()
}

#[component]
fn Binary(a: u16, b: u16, c: u16) -> View {
    let x = a ^ b;
    reactive_assert!(a, b => x);

    let y = b & c;
    reactive_assert!(b, c => y);

    let z = x * y;
    reactive_assert!(a, b, c => z; a, b => x; b, c => y);

    ().into()
}

#[component]
fn Block(a: u8, b: u8) -> View {
    let mut y = 0;
    let x = {
        let x = a + b;
        y += *b;
        x
    };
    reactive_assert!(a, b => x; b => y);

    ().into()
}

#[component]
fn FnCall(a: u8) -> View {
    let b = std::convert::identity(a);
    reactive_assert!(a => b);
    
    ().into()
}

#[component]
fn Cast(a: u8) -> View {
    let ret = *a as u16;
    reactive_assert!(a => ret);
    
    ().into()
}

// TODO: fix closure handling
#[component]
fn Closure(a: u8, b: u8) -> View {
    let closure1 = || {
        let (mut a, b) = (*a, *b);
        a += b;
    };

    reactive_assert!(a, b => closure1);

    let closure2 = || {
        a;
    };
    reactive_assert!(a => closure2);

    ().into()
}

#[component]
fn Field(a: (u8, u8), b: u8) -> View {
    let ret = a.0;
    reactive_assert!(a => ret);
    //test assigning

    let mut a_copy = *a;
    a_copy.1 = *b;
    reactive_assert!(b => a_copy);

    ().into()
}

#[component]
fn ForLoop(a: u8, b: u8) -> View {
    let mut x = 0;

    for n in 0..1 {
        let n = b;
        x += n;
    };
    reactive_assert!(b => x);

    ().into()
}

#[component]
fn If(a: u8, b: u8, c: u8) -> View {
    let x = if *a == 0 {
        b
    }
    else {
        c
    };
    reactive_assert!(a, b, c => x);

    ().into()
}

//TODO: fix body processing
// #[component]
// fn _loop(a: u8, b: u8, c: u8) {
//     let x = loop {
//         if *a == 0 {
//             break b; 
//         };
//         break c;
//     };

//     reactive_assert!(a, b, c => x);

//     ().into()
// }

#[component]
fn Match(a: u8, b: u8, c: u8, d: u8) -> View {
    let option = Some(a);
    let x = match option {
        Some(var) => var,
        None => b
    };
    reactive_assert!(a, b => x);

    let y = match x {
        0 => c,
        1 => d,
        _ => a
    };

    reactive_assert!(a, b, c, d => y);

    ().into()
}

#[component]
fn Unary(a: u8) -> View {
    let b = !a;
    reactive_assert!(a => b);

    ().into()
}

#[component]
fn Tuple(a: u8, b: u8, c: u8) -> View {
    let tuple = (a, b, c);
    reactive_assert!(a, b, c => tuple);
    {
        let first = tuple.0;
        let second = tuple.1;
        let third = tuple.2;
        reactive_assert!(a => first; b => second; c => third);
    }

    ().into()
}

#[component]
fn While(a: u8, b: u8) -> View {
    let mut x = 0;
    while *a == 0 {
        x = *b;
    };
    reactive_assert!(b => x);

    ().into()
}

#[component]
fn Enclose(a: u8) -> View {
    let b = enclose!(a; a);
    reactive_assert!(a => b);
    ().into()
}

#[component]
fn StdMacros(a: u8, b: u8, c: u8) -> View {
    // testing dbg!
    let a_prime = dbg!(a);
    reactive_assert!(a => a_prime);
    let ab_prime = dbg!(a, b);
    reactive_assert!(a, b => ab_prime);

    // testing format!
    let formatted = format!("{} {} {d}", a, b, d=c);
    reactive_assert!(a, b, c => formatted);

    // testing matches!
    let matched = matches!(a, 1);
    reactive_assert!(a => matched);
    let matched = matches!(b, 1 | 2);
    reactive_assert!(b => matched);
    let matched = matches!(a, 3 | 4 if *b > 2,);
    reactive_assert!(a, b => matched);

    // testing vec!
    let vec = vec![a];
    reactive_assert!(a => vec);
    let vec = vec![b, c];
    reactive_assert!(b, c => vec);
    let vec = vec![a; *c as usize];
    reactive_assert!(a, c => vec);

    // testing try!
    let ret: Result<u8, ()> = (|| {
        let a = Ok(*a);
        Ok(r#try!(a))
    })();
    reactive_assert!(a => ret);
    
    ().into()
}