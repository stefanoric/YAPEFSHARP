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

let square x = x * x

// a² + b²  + ... + z²
let sum_of_square max =
    Seq.unfold (fun x -> if x > max then None else Some(x*x, x+1)) 1
    |> Seq.sum

// (a +b + ... + z)²
let square_of_sum max =
    {1 .. max}
    |> Seq.sum
    |> square

let problem_6 max =
    square_of_sum(max) - sum_of_square(max)
