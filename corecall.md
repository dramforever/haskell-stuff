# What happens when you `foreign import "wrapper"`?

As you might know, you can convert a Haskell function into a C function pointer, if you `foreign import "wrapper"` a function that does so.

```haskell
foreign import "wrapper" wrapper :: (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))
```

What happens when you call `wrapper fn`?

## A short summary of the procedure

In your program two things are added:

1. A C function that, given C-friendly arguments, can call a Haskell function and unwrap the result back to C-friendly form.
2. `wrapper` itself.

And it when it is time for `wrapper fn`:

1. A `StablePtr` of `fn` is created, which tells the GC not to garbage collect the function.
2. An *adjustor* is created by dynamically writing machine code that calls the C function with the `StablePtr` and more arguments from C. Basically a closure in machine code.
3. The address to the machine code of the *adjustor* is wrapped in a `FunPtr` and returned.

## A breakdown of some of the code involved

### A C function

GHC generates, this C function, and compiles/links it into your program: (Get this with `-ddump-foreign`.)

```c
HsInt zdmainzdMainzdMainzuwrapper(StgStablePtr the_stableptr, HsInt a1)
{
    Capability *cap;
    HaskellObj ret;
    HsInt cret;

    // Wait for and grab a Haskell capability:
    cap = rts_lock();

    // Run the function passed in by the StablePtr and force the result Int to whnf:
    rts_evalIO(
        &cap,
        rts_apply(
            cap,

            // GHC.TopHandler.runIO adds an exception handler to an IO function.
            (HaskellObj) runIO_closure,
            rts_apply(
                cap,
                (StgClosure*) deRefStablePtr(the_stableptr),
                rts_mkInt(
                    cap,

                    // In case you already forgot or never noticed, this is the argument.
                    a1))),
        &ret);

    // Check, for example, whether we have been killed or interrupted, and if so exit:
    rts_checkSchedStatus("zdmainzdMainzdMainzuwrapper", cap);

    // Get the x from an evaluated (I# x):
    cret = rts_getInt(ret);

    // Okay we're done with the capability:
    rts_unlock(cap);

    return cret;
}
```

(Here's `rts_getInt` in `rts/RtsAPI.c`.)

```c
HsInt
rts_getInt (HaskellObj p)
{
    return (HsInt)(UNTAG_CLOSURE(p)->payload[0]);
}
```

### A generated Haskell function

(Get this with `-ddump-simpl`.)

```haskell
wrapper
  = \ function srw1 ->

      -- Create a StablePtr# of 
      case makeStablePtr# function srw1 of
      { (# srw2, stablePtr #) ->

      -- Create the function pointer
      case {__pkg_ccall Int#
             -> StablePtr# (Int -> IO Int)
             -> Addr#
             -> Addr#
             -> State# RealWorld
             -> (# State# RealWorld, Addr# #)}_d1jo
             1#
             stablePtr
             (__label "zdmainzdMainzdMainzuwrapper" (function))
             main4
             srw2
      of
      { (# srw3, fptr #) ->

      -- And wrap it in a constructor
      (# srw3, FunPtr fptr #)
      }
      }
```

A `__pkg_ccall` to what? C-- reveals the answer:

(Get this from `-ddump-cmm`.)

```cmm
 c1Jy:
     unwind Sp = Just Sp + 8;
     (_s1J8::I64) = call "ccall" arg hints:  [‘signed’, PtrHint,
                                              PtrHint,
                                              PtrHint]  result hints:  [PtrHint] createAdjustor(1, R1, zdmainzdMainzdMainzuwrapper, main4_bytes);
```

It's actually a call to `createAdjustor`, basically `createAdjustor(1, R1, zdmainzdMainzdMainzuwrapper, main4_bytes)`. 

By the way, `main4` is:

(`-ddump-simpl`)

```haskell
main4 :: Addr#
main4 = "L"#
```

Which is basically:

```c
char main4_bytes = "L";
```

### What exactly is an adjustor

An adjustor is basically a piece of *dynamically created machine code* that arranges a call to the C function mentioned above. It is created as executable memory, and a pointer to it looks like a function pointer like, for example, `int (*)(int)` in our case.

`createAdjustor` can be found in `rts/Adjustor.c`. The prototype of `createAdjustor` is this:

```c
void* createAdjustor (int cconv, StgStablePtr hptr, StgFunPtr wptr, char *typeString);
```

(Note that the `libffi` version of this function is probably not used, at least by default, as I cannot find `USE_LIBFFI_FOR_ADJUSTORS` anywhere else, and the wrapper has a wrong type. Try starting by finding `#else // To end of file...`.)

`cconv` is `1`, which means `ccall`. (It can also be `0` non-`darwin` x86, which means `stdcall`).

(The non-`libffi` version of) `createAdjustor` is a monstrosity, with more than a thousand lines in its implementation, mainly due to the need to cater for all the architectures GHC needs to support.

Freeing such a function pointer, then, involves finding and freeing the `StablePtr` hiding among the instructions, and deallocating the memory containing the machine code.

### What is a `StablePtr` anyway

The documentation for `Foreign.StablePtr` says it all, really:

> A stable pointer is a reference to a Haskell expression that is guaranteed not to be affected by garbage collection, i.e., it will neither be deallocated nor will the value of the stable pointer itself change during garbage collection (ordinary references may be relocated during garbage collection). 

How did we do this? In `rts/Stable.c`:

> Our solution is to keep a table of all objects we've given to the C-world and to make sure that the garbage collector collects[1] these objects --- updating the table as required to make sure we can still find the object.

([1]: I think 'collects' means 'sees and doesn't free', instead of 'frees'.)

So basically, a `StablePtr` is an entry in some global table of 'please don't free me' things. And you can't really reliably get the underlying pointer to the Haskell value in C because you weren't supposed to know what it was.

And I didn't copy the code here, but that's it. `stable_ptr_table` is just an array of pointers, which automatically expands as needed. The pointer also doubles as a linked list of free entries. The GC checks this table and knows that the values pointed to are still accessible.
