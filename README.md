# clsh

## What's This?
This is the shell written by common lisp which was inspired by eshell on emacs.
The purpose is it realizes we can use common lisp everywhere. 
*Notice*, you have to know this product is alpha status. You might lose important data or work by accidental crash!

## Supported CL implemnts
Only sbcl. But if you reqeust, I will support it.

## Ready To Start
You need below libraries to use.
- GNU readline library

And you need below common lisp libraries. Maybe Quicklisp help to install.
- maxpc
- cl-fad
- alexandria
- cl-readline
- cl-ppcre

After clone this, execute below in the directory.
```bash
$ sbcl --load clsh.lisp --eval '(clsh:run)'
```

## How To Use
When you run clsh, a prompt will appear and you can execute some commands and common lisp expression as you like.
```
~/work/:[COMMON-LISP-USER]>
~/work/:[COMMON-LISP-USER]> ls | sort
aaa bbb ...
~/work/:[COMMON-LISP-USER]> (describe 'func) | less
...
```

When executing single common lisp expression, it's executed same process like this.
This means you can change environment variables or common lisp variable on the clsh process.
```
~/work/:[COMMON-LISP-USER]> (defvar *test-var* 1)
~/work/:[COMMON-LISP-USER]> (print *test-var*)
1
~/work/:[COMMON-LISP-USER]> (sb-posix:setenv "TEST" "ok" 1)
~/work/:[COMMON-LISP-USER]> (sb-posix:getenv "TEST")
"ok"
```

But if you use pipe function, clsh produce new process for executing.
```
~/work/:[COMMON-LISP-USER]> (defvar *test-var* 1) | cat
~/work/:[COMMON-LISP-USER]> (print *test-var*)
error!!
```

## Status
- editable command line.
- completion for lisp expressions or command & files.
- have a lot of bugs.

