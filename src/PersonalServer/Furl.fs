namespace Jackfoxy.PersonalServer

open System
open System.Net.Http

module Furl =

// MIT License

// Copyright (c) 2017 Mark Seemann

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

    let private addHeader (headers : Headers.HttpHeaders) (name, value : string) =
        headers.Add (name, value)

    let private addBody (req : HttpRequestMessage) headers body =
        req.Content <- new StringContent (body)
        let contentTypeHeader =
            headers |> List.tryFind (fun (n, _) -> n = "Content-Type")
        contentTypeHeader
        |> Option.iter (fun (_, v) -> req.Content.Headers.ContentType.MediaType <- v)

    let result (t : System.Threading.Tasks.Task<_>) = t.Result

    let composeMessage meth (url : Uri) headers body =
        let req = new HttpRequestMessage (meth, url)
        Option.iter (addBody req headers) body

        headers
        |> List.partition (fun (n, _) -> n = "Content-Type")
        |> snd
        |> List.iter (addHeader req.Headers)
        req

    let get url headers =
        use client = new HttpClient ()
        // HttpMethod is qualified to avoid collision with FSharp.Data.HttpMethod,
        // if FSharp.Data is imported in a script as well as Furl.
        composeMessage Net.Http.HttpMethod.Get (Uri url) headers None
        |> client.SendAsync
        |> result

    let post url headers body =
        use client = new HttpClient ()
        // HttpMethod is qualified to avoid collision with FSharp.Data.HttpMethod,
        // if FSharp.Data is imported in a script as well as Furl.
        composeMessage Net.Http.HttpMethod.Post (Uri url) headers (Some body)
        |> client.SendAsync
        |> result

    let bodyText (resp : HttpResponseMessage) =
        resp.Content.ReadAsStringAsync().Result

    let headersAndBodyText (resp : HttpResponseMessage) = 
        let x = 
            resp.Headers |> Seq.cast<Collections.Generic.KeyValuePair<string, Collections.Generic.IEnumerable<string>>>
        let statusCode = resp.StatusCode
        statusCode, x, resp.Content.ReadAsStringAsync().Result