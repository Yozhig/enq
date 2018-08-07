# Native implemented queues for Erlang

FIFO and LIFO queues with TTL implemented in NIFs.

## Why

### Advantages

* Written in NIFs, so it's **fast**
* Queues have a time-to-live mechanism

### Caveats

* Written in NIFs, so it's dangerous
* Can block a scheduler when there are too many items to be cleaned up due to TTL (may be changed in future)
* No internal locks and checks for owner of a queue, so it's up to user to control this (may be changed in future)

## Requirements

* GNU C Compiler
* Make or rebar
* Erlang/OTP R15+

## How to use

Add `enq` as a dependency to your Makefile and/or rebar.config (depends on the build system you are using).

The function `enq:new/0` creates FIFO queue without ttl or max_size restrictions.
To create a queue with some parameters, call:
```
{ok, Q} = enq:new([lifo, % creates LIFO queue
                   {ttl, 10000}, % with 10 seconds TTL
                   {max_size, 50000}]). % and maximum of 50000 elements

```
Push some data to the queue:
```
ok = enq:push(Q, test).
```
Pop one element from the queue:
```
[test] = enq:pop(Q).
```
Get size of the queue in O(1):
```
N = enq:size(Q).
```