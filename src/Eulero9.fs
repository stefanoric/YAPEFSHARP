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
let simple_triplet_sequence =
    Seq.initInfinite (fun x -> (2*x+1, 2*x*(x+1), 2*x*(x+1)+1))
    |> Seq.filter (fun (a,b,c) -> a+b+c = 1000)

let triplets top =
    [ for m in 1 .. top do
          for n in 1 .. m-1 do
             let a = m*m-n*n
             let b = 2*m*n
             let c = m*m+n*n
             yield [a;b;c] ]
    
let findTriplet top =
    triplets top
    |> List.find (fun [a;b;c] -> a+b+c=1000)

let multiplyList list =
    List.fold (fun acc elem -> acc*elem) 1 list


let problem_9 () =
    find_triplet 1000
    |> multiplyList

