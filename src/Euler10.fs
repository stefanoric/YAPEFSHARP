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

// SOLUTION 1: TRIAL DIVISION
let is_prime (x:int64) =
    let rec check i =
        double i > sqrt (double x) || (x % i <> 0L && check (i + 1L))
    check 2L  

let trial_division (n:int64) =
    seq { for i in 2L .. n do
          if is_prime i then yield i }
    |> Seq.takeWhile (fun x -> x < n)
    |> Seq.sum

// SOLUTION 2: USE THE SIEVE
let sieve (n:int64) =
    let candidatePrimes = new Dictionary<int64, bool>()
    for i in 2L .. n do candidatePrimes.Add(i, true)
    for i in 2L .. n/2L do
        if candidatePrimes.[i] = true then
            let mutable j = i
            while (j + i <= n) do
                j <- j + i
                candidatePrimes.[j] <- false
    seq { for i in 2L .. n do
            if candidatePrimes.[i] = true then
                yield i }
    |> Seq.sum
