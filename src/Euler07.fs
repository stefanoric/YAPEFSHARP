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

let is_prime x =
    let rec check i =
        x > 1L &&
       (double i > sqrt (double x) || (x % i <> 0L && check (i + 1L)))
    check 2L                 

let sequence_of_first_n_primes n =
    Seq.initInfinite (fun x -> int64 x)
    |> Seq.filter is_prime
    |> Seq.take n
    |> Seq.max
