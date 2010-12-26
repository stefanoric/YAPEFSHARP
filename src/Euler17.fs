// Copyright 2010, Stefano Ricciardi - www.stefanoricciardi.com
//
// This is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of
// the License, or (at your option) any later version.

// This software is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.

// You should have received a copy of the GNU Lesser General Public
// License along with this software; if not, write to the Free
// Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
// 02110-1301 USA, or see the FSF site: http://www.fsf.org.

#light

let thousand    = "oneThousand"
let hundred     = "hundred"
let hundred_and = "hundredAnd"

let zero_to_nine = new System.Collections.Generic.Dictionary<int, string>()
zero_to_nine.[0] <- ""
zero_to_nine.[1] <- "one"
zero_to_nine.[2] <- "two"
zero_to_nine.[3] <- "three"
zero_to_nine.[4] <- "four"
zero_to_nine.[5] <- "five"
zero_to_nine.[6] <- "six"
zero_to_nine.[7] <- "seven"
zero_to_nine.[8] <- "eight"
zero_to_nine.[9] <- "nine"

let ten_to_nineteen =  new System.Collections.Generic.Dictionary<int, string>()
ten_to_nineteen.[10] <- "ten"
ten_to_nineteen.[11] <- "eleven"
ten_to_nineteen.[12] <- "twelve"
ten_to_nineteen.[13] <- "thirteen"
ten_to_nineteen.[14] <- "fourteen"
ten_to_nineteen.[15] <- "fifteen"
ten_to_nineteen.[16] <- "sixteen"
ten_to_nineteen.[17] <- "seventeen"
ten_to_nineteen.[18] <- "eighteen"
ten_to_nineteen.[19] <- "nineteen"

let twenty_to_ninety = new System.Collections.Generic.Dictionary<int, string>()
twenty_to_ninety.[20] <- "twenty"
twenty_to_ninety.[30] <- "thirty"
twenty_to_ninety.[40] <- "forty"
twenty_to_ninety.[50] <- "fifty"
twenty_to_ninety.[60] <- "sixty"

twenty_to_ninety.[70] <- "seventy"
twenty_to_ninety.[80] <- "eighty"
twenty_to_ninety.[90] <- "ninety"

let len n = String.length n

let calculate_letters_under_one_hundred n =
    match n with
        | x when x < 10 -> len zero_to_nine.[x]
        | x when x < 20 -> len ten_to_nineteen.[x]
        | x when x < 100 -> len twenty_to_ninety.[((x / 10) * 10)] + len zero_to_nine.[(x % 10)]
        | _ -> failwith "Number is too big!"

let calculate_letters n =
    match n with
        | x when x < 100 -> calculate_letters_under_one_hundred x
        | x when ((x < 1000) && (x % 100 = 0)) -> len zero_to_nine.[(x / 100)] + len hundred
        | x when x < 1000 -> len zero_to_nine.[(x / 100)] + len hundred_and +  calculate_letters_under_one_hundred (x % 100)
        | 1000 -> len thousand
        | _ -> failwith "Numer is too big!"

let all_letters_up_to n =
    List.sumBy (fun i -> calculate_letters i) [1..n]
        

