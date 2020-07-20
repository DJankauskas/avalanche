//TODO: update functions to be valid component invocations, and allow its use in this crate

// use super::{component, reactive_assert};

// struct HasFields {
//     field_one: u8,
//     field_two: u8
// }

// #[component]
// fn _identity(a: u8) -> u8 {
//     reactive_assert!(a => a);
//     let a = a;
//     reactive_assert!(a => a);
//     a
// }

// #[component]
// fn _array_index(a: u8, b: u8, c: u8) {
//     let arr = [a, b, c];
//     reactive_assert!(a, b, c => arr);

//     {
//         let first = arr[0];
//         let second = arr[1];
//         let third = arr[2];
//         reactive_assert!(a => first; b => second; c => third);
//     }

//     let arr2 = [a; 3];
//     reactive_assert!(a => arr2);
//     let from2 = arr2[0];
//     reactive_assert!(a => from2);


//     let indexed = arr2[c as usize];
//     reactive_assert!(c => indexed);
// }

// #[component]
// fn _assign(a: u8, b: u8, mut has_fields: HasFields) {
//     let mut x = a;
//     reactive_assert!(a => x);
//     x = b;
//     reactive_assert!(b => x);

//     x = a;
//     reactive_assert!(a => x);

//     x += b;
//     reactive_assert!(a, b => x);

//     has_fields.field_one = a;
//     reactive_assert!(a => has_fields);
// }

// #[component]
// fn _binary(a: u16, b: u16, c: u16) {
//     let x = a ^ b;
//     reactive_assert!(a, b => x);

//     let y = b & c;
//     reactive_assert!(b, c => y);

//     let z = x * y;
//     reactive_assert!(a, b, c => z; a, b => x; b, c => y);
// }

// #[component]
// fn _block(mut a: u8, b: u8) {
//     let x = {
//         let x = a + b;
//         a += b;
//         x
//     };
//     reactive_assert!(a, b => x; b => a);
// }

// #[component]
// fn _fn_call(a: u8) -> u8 {
//     let b = _identity(a);
//     reactive_assert!(a => b);
//     b
// }

// #[component]
// fn _cast(a: u8) -> u16 {
//     let ret = a as u16;
//     reactive_assert!(a => ret);
//     ret
// }

// // //TODO: fix closure handling
// // #[reactive]
// // fn _closure(a: u8, b: u8) -> u8 {
// //     let closure = || {
// //         a;
// //     };

// //     reactive_assert!(a => closure);
// // }

// #[component]
// fn _field(mut a: (u8, u8), b: u8) -> u8 {
//     let ret = a.0;
//     reactive_assert!(a => ret);
//     //test assigning
//     a.1 = b;
//     reactive_assert!(b => a);
//     ret
// }

// #[component]
// fn _for_loop(mut a: u8, b: u8) {
//     for n in 0..1 {
//         let n = b;
//         a += n;
//     };
//     reactive_assert!(b => a);
// }

// #[component]
// fn _if(a: u8, b: u8, c: u8) {
//     let x = if a == 0 {
//         b
//     }
//     else {
//         c
//     };
//     reactive_assert!(a, b, c => x);
// }

// //TODO: fix body processing
// // #[reactive]
// // fn _loop(a: u8, b: u8, c: u8) {
// //     let x = loop {
// //         if a == 0 {
// //             break b; 
// //         };
// //         break c;
// //     };

// //     reactive_assert!(a, b, c => x);
// // }

// #[component]
// fn _match(a: u8, b: u8, c: u8, d: u8) {
//     let option = Some(a);
//     let x = match option {
//         Some(var) => var,
//         None => b
//     };
//     reactive_assert!(a, b => x);

//     let y = match x {
//         0 => c,
//         1 => d,
//         _ => a
//     };

//     reactive_assert!(a, b, c, d => y);
// }

// #[component]
// fn _unary(a: u8) {
//     let b = !a;
//     reactive_assert!(a => b);
// }

// #[component]
// fn _tuple(a: u8, b: u8, c: u8) {
//     let tuple = (a, b, c);
//     reactive_assert!(a, b, c => tuple);
//     {
//         let first = tuple.0;
//         let second = tuple.1;
//         let third = tuple.2;
//         reactive_assert!(a => first; b => second; c => third);
//     }
// }

// #[component]
// fn _while(a: u8, b: u8) {
//     let mut x = 0;
//     while a == 0 {
//         x = b;
//     };
//     reactive_assert!(b => x);
// }

// #[component]
// fn _temp_macro(a: u8) {
//     let a = Test!{
//         hello: "there"
//     };
// }