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

let rec is_palindromic (a:string) =
    let length = a.Length;
    match a with
        | ""  -> true
        | x -> match length with
                 | 1 -> true
                 | _ -> x.[0] = x.[length-1] &&
                       is_palindromic (x.Substring(1,(length - 2)))

let max_palindrome =
    let numbers = seq {
        for i in 100 .. 999 do
            for j in 100 .. 999 do
                if is_palindromic(Convert.ToString(i*j)) then yield (i*j) }
    Seq.max numbers
