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

// greatest common divisor (Euclid Algorithm)
let rec gcd (a:int64, b:int64) =
    match b with
       | x when x = 0L -> a
       | _ -> gcd (b, a % b)

// least common divisor
let rec lcm (a:int64, b:int64) =
    (a * b) / gcd (a,b)

// least common divisor for a list of numbers
let rec lcmlist (list: int64 list) =
    match list with
        | [] -> 1L
        | [a] ->  a
        | [a;b] ->  lcm(a,b)
        | h::t  ->  lcm(h, lcmlist t)

// utility to generate a list of all integers
// up to a certain number
let generateListTo top =
    let l = [for i in 1L .. int64 top ->  i]
    l

let resolveProblem5 =
    generateListTo 20 |>  lcmlist
