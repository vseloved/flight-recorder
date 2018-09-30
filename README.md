# flight-recorder (frlog) - a robust REPL logging facility

![frlog logo](frlogo.png)

Interactivity is a principal requirement for a usable programming environment.
Interactivity means that there should be a shell/console/REPL or other
similar text-based command environment. And a principal requirement for
such an environment is keeping history. And not just keeping it,
but doing it robustly:

- recording history from concurrently running sessions
- keeping unlimited history
- identifying the time of the record and its context

This allows to freely experiment and reproduce the results of successful
experiments, as well as go back to an arbitrary point in time and take another
direction of your work, as well as keeping DRY while performing common
repetitive tasks in the REPL (e.g. initialization of an environment or context).

`flight-recorder` or `frlog` (when you need a distinct name) is a small tool
that intends to support all these requirements. It grew out of a frustration
with how history is kept in [SLIME][1], so it was primarily built to support
this environment, but it can be easily utilized for other shells that don't
have good enough history facility. It is possible due to its reliance on
the most common and accessible data-interchange facility: text-based HTTP.


## Mechanics

`frlog` is a backend service that supports any client that is able to send
an HTTP request.

## Backend

![Lisp logo](alien.png)

The backend is a Common Lisp script that can be run in the following manner
(probably, the best way to do it is inside [screen][2]):

```
sbcl --noprint --load hunch.lisp -- -port 7654 -script flight-recorder.lisp
```

If will print a bunch of messages that should end with the following line
(modulo timestamp):

```
[2018-09-29 16:00:53 [INFO]] Started hunch acceptor at port: 7654.
```

The service appends each incoming request to the text file in markdown format:
`~/.frlog.md`.

The API is just a single endpoint - `/frlog` that accepts GET and POST requests.
The parameters are:

- `text` is the content (url-encoded, for sure) of the record
  that can, alternatively, be sent in the POST request's body (more robust)

Optional query parameters are:

- `title` - used to specify that this is a new record: for console-based
  interactions, usually, there's a command and zero or more results -
  a command starts the record (and thus should be accompanied with the title:
  for SLIME interactions it's the current Lisp package and a serial number).
  An entitled record is added in the following manner:
  ```
  ### cl-user (10) 2018-09-29_15:49:17

      (uiop:pathname-directory-pathname )
  ```
  If there's no title, the text is added like this:
  ```
  ;;; 2018-09-29_15:49:29

      #<program-error @ #x100074bfd72>
  ```
- `tag` - if provided it signals that the record should be made not to
  a standard `.frlog.md` file, but to `.frlog-<tag>.md`. This allows
  to easily log a specific group of interactions separately

If the response code is 200 everything's fine.


### Clients

Currently, 2 clients are available:

- a SLIME client `flight-recorder.el` (just load it from Emacs if you have
  SLIME initialized)
- and a tiny Lisp client `frlog.lisp`

A SLIME client has a configuration variable `*frlog-port*`.
There are also 2 interactive commands: `frlog-start-session`
(that will prompt for the session tag, which will be sent to the server)
and `frlog-end-session`.


## License

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
 
 - Redistributions of source code must retain this copyright
   notice, this list of conditions, and the following disclaimer.

 - Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

 - Neither the name of Edward Marco Baringer, nor BESE, nor the names
   of its contributors may be used to endorse or promote products
   derived from this software without specific prior written permission.
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



[1]: https://common-lisp.net/project/slime/
[2]: https://www.gnu.org/software/screen/
