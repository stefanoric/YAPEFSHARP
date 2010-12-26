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

let divides y x =
    x % y = 0

let rec all_factors_slow_rec n i acc =
    match i with
        | 1 -> 1::acc
        | x when divides x n -> all_factors_slow_rec n (x-1) (x::acc) 
        | _ -> all_factors_slow_rec n (i-1) acc

let rec all_factors_quick_rec n i factors =
    if divides i n then
        let y = n / i
        if (i < y) then
            all_factors_quick_rec n (i + 1) (i::y::factors)
        elif (i = y) then
            // we have reached the square root value
            i::factors
        else
            factors
    elif i > int (sqrt (float n)) then
        // we are beyond the square root value
        factors
    else
        // try with the next number
        all_factors_quick_rec n (i + 1) factors
        
let all_factors_slow n =
    all_factors_slow_rec n n []

let all_factors_quick n =
    all_factors_quick_rec n 1 []

let triangles =
    Seq.unfold (fun (acc, state) -> Some (acc, (state + acc, state + 1))) (0, 1)
    |> Seq.skip 1 // skip the initial 0

let find_index m =
    triangles
    |> Seq.map all_factors_slow
    |> Seq.tryFindIndex (fun x -> List.length x >= m)

let resolve_problem_12 m =
    let i = find_index m
    match i with
        | None -> failwith "Cannot resolve problem"
        | Some(i) ->
            let list = triangles |> Seq.take (i+1) |> List.ofSeq
            list.[i]
