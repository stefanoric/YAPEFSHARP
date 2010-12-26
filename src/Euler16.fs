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

open System
open System.Numerics

let rec decompose_rec n power_of_ten accu =
    match power_of_ten with
        | i when i = 1I -> accu @ [n]
        |_ ->
            let d = n / power_of_ten
            let digits = accu @ [d]
            decompose_rec (n - (d * power_of_ten)) (power_of_ten / 10I) digits

let decompose n =
    let power = floor (log10 (float n))
    let initial_power_of_ten = BigInteger.Pow(10I, int32 power)
    decompose_rec n initial_power_of_ten []

let sum_digits n =
    decompose n |> List.sum
