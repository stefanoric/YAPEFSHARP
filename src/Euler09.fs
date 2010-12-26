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

// generate triplets using Euclid's Formula
let pythagorean_triplets top =
    [ for m in 1 .. top do
          for n in 1 .. m-1 do
          let a = m*m-n*n
          let b = 2*m*n
          let c = m*m+n*n
          yield [a;b;c] ]

// multiply all the values of a list
let multiply_list list =
    List.fold (fun acc elem -> acc*elem) 1 list

// find a triplet where the sum of values
// is equal to a given number
let find_triplet_with_sum sum =
    pythagorean_triplets sum
    |> List.find (fun [a;b;c] -> a+b+c=sum)

let problem_9 () =
    find_triplet_with_sum 1000
    |> multiply_list
