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

open System.Collections.Generic

let is_odd x = 
    x % 2L = 1L

let rec naive_sequence n (partial:int64 list) =
    match n with
        | 1L -> partial @ [1L]
        | x when is_odd x -> naive_sequence ((3L*x) + 1L) (partial @ [n])
        |_ -> naive_sequence (n/2L) (partial @ [n])


let rec collatz_sequence n (partial:int64 list) (cache: IDictionary<int64, int64 list>) =
    if cache.ContainsKey(n) then
        let result = partial @ cache.[n]
        let sequence_starter = List.head result
        if not(cache.ContainsKey(sequence_starter)) then
            cache.Add(sequence_starter, result) |> ignore
        result 
    else
        match n with
        | 1L ->
            let result = partial @ [1L]
            let sequence_starter = List.head result
            cache.Add(sequence_starter, result) |> ignore
            result 
        | n when is_odd n -> collatz_sequence ((3L*n) + 1L) (partial @ [n]) cache
        | _ -> collatz_sequence (n/2L) (partial @ [n]) cache

let sequence_cache = new Dictionary<int64, int64 list>()

let calculate_sequence n =
    collatz_sequence n [] sequence_cache

let max_sequence_lengths (max:int64) =
    let all_sequences = [for i in 1L..max -> calculate_sequence i]
    let lengths = List.map List.length all_sequences
    let max = List.max lengths
    let max_index = List.findIndex (fun (x:int64 list) -> x.Length = max) all_sequences
    max_index + 1
