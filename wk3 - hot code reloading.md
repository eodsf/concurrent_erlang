Notes :
To demonstrate hot code reloading, I used the provided frequency.erl & frequency3.erl.
(frequency3.erl differed in that it added ability to inject new frequencies).

The original source was compiled, the server started, and requests made until it
ran out of frequencies, as can be seen below.

Next, the source for the newer version was copied to frequency.erl, and that compiled.
This results in the new version being loaded into the BEAM.
[Note that in order to facilitate this, the module must using fully qualified calls to functions
(eg, using the module name) ].

The call to 'code:soft_purge' will purge older versions of the same module if they are not bring 
used, as is the case here (after the new version is loaded).


1> c(frequency).
{ok,frequency}
2> frequency:start().
true
3> frequency:allocate().
{ok,10}
4> frequency:allocate().
{ok,11}
5> frequency:allocate().
{ok,12}
6> frequency:allocate().
{ok,13}
7> frequency:allocate().
{ok,14}
8> frequency:allocate().
{ok,15}
9> frequency:allocate().
{error,no_frequency}
10> c(frequency).
{ok,frequency}
11> frequency:allocate().
{error,no_frequency}
12> % the re-compile above is the frequency3.erl code copied into the frequency.erl
.
* 2: syntax error before: '.'
12> code:soft_purge(frequency).
true
13> frequency:inject([16,17,18]).
injected
14> frequency:allocate().
{ok,16}
15> frequency:allocate().
{ok,17}
16> frequency:allocate().
{ok,18}
17> frequency:allocate().
{error,no_frequency}
18>

