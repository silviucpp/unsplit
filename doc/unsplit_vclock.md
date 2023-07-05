

# Module unsplit_vclock #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A simple Erlang implementation of vector clocks as inspired by Lamport logical clocks.

Copyright (c) 2007-2008 Basho Technologies

__Authors:__ Justin Sheehy ([`justin@basho.com`](mailto:justin@basho.com)), Andy Gross ([`andy@basho.com`](mailto:andy@basho.com)).

__References__* Leslie Lamport (1978). "Time, clocks, and the ordering of events in a distributed system". Communications of the ACM 21 (7): 558-565.
* Friedemann Mattern (1988). "Virtual Time and Global States of Distributed Systems". Workshop on Parallel and Distributed Algorithms: pp. 215-226

<a name="types"></a>

## Data Types ##




### <a name="type-counter">counter()</a> ###


<pre><code>
counter() = integer()
</code></pre>




### <a name="type-timestamp">timestamp()</a> ###


<pre><code>
timestamp() = integer()
</code></pre>




### <a name="type-vc_entry">vc_entry()</a> ###


<pre><code>
vc_entry() = {<a href="#type-vnode">vnode()</a>, {<a href="#type-counter">counter()</a>, <a href="#type-timestamp">timestamp()</a>}}
</code></pre>




### <a name="type-vclock">vclock()</a> ###


<pre><code>
vclock() = [<a href="#type-vc_entry">vc_entry()</a>]
</code></pre>

The timestamp is present but not used, in case a client wishes to inspect it.
Nodes can have any term() as a name, but they must differ from each other.



### <a name="type-vnode">vnode()</a> ###


<pre><code>
vnode() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_nodes-1">all_nodes/1</a></td><td>Return the list of all nodes that have ever incremented VClock.</td></tr><tr><td valign="top"><a href="#descends-2">descends/2</a></td><td>Return true if Va is a direct descendant of Vb, else false -- remember, a vclock is its own descendant!</td></tr><tr><td valign="top"><a href="#equal-2">equal/2</a></td><td>Compares two VClocks for equality.</td></tr><tr><td valign="top"><a href="#fresh-0">fresh/0</a></td><td>Create a brand new vclock.</td></tr><tr><td valign="top"><a href="#get_counter-2">get_counter/2</a></td><td>Get the counter value in VClock set from Node.</td></tr><tr><td valign="top"><a href="#get_timestamp-2">get_timestamp/2</a></td><td>Get the timestamp value in a VClock set from Node.</td></tr><tr><td valign="top"><a href="#increment-2">increment/2</a></td><td>Increment VClock at Node.</td></tr><tr><td valign="top"><a href="#merge-1">merge/1</a></td><td>Combine all VClocks in the input list into their least possible common descendant.</td></tr><tr><td valign="top"><a href="#prune-3">prune/3</a></td><td>Possibly shrink the size of a vclock, depending on current age and size.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_nodes-1"></a>

### all_nodes/1 ###

<pre><code>
all_nodes(VClock::<a href="#type-vclock">vclock()</a>) -&gt; [<a href="#type-vnode">vnode()</a>]
</code></pre>
<br />

Return the list of all nodes that have ever incremented VClock.

<a name="descends-2"></a>

### descends/2 ###

<pre><code>
descends(Va::<a href="#type-vclock">vclock()</a>, Vb::<a href="#type-vclock">vclock()</a>) -&gt; boolean()
</code></pre>
<br />

Return true if Va is a direct descendant of Vb, else false -- remember, a vclock is its own descendant!

<a name="equal-2"></a>

### equal/2 ###

<pre><code>
equal(VClockA::<a href="#type-vclock">vclock()</a>, VClockB::<a href="#type-vclock">vclock()</a>) -&gt; true | false
</code></pre>
<br />

Compares two VClocks for equality.
Not very fast.

<a name="fresh-0"></a>

### fresh/0 ###

<pre><code>
fresh() -&gt; <a href="#type-vclock">vclock()</a>
</code></pre>
<br />

Create a brand new vclock.

<a name="get_counter-2"></a>

### get_counter/2 ###

<pre><code>
get_counter(Node::<a href="#type-vnode">vnode()</a>, VClock::<a href="#type-vclock">vclock()</a>) -&gt; <a href="#type-counter">counter()</a>
</code></pre>
<br />

Get the counter value in VClock set from Node.

<a name="get_timestamp-2"></a>

### get_timestamp/2 ###

<pre><code>
get_timestamp(Node::<a href="#type-vnode">vnode()</a>, VClock::<a href="#type-vclock">vclock()</a>) -&gt; <a href="#type-timestamp">timestamp()</a>
</code></pre>
<br />

Get the timestamp value in a VClock set from Node.

<a name="increment-2"></a>

### increment/2 ###

<pre><code>
increment(Node::<a href="#type-vnode">vnode()</a>, VClock::<a href="#type-vclock">vclock()</a>) -&gt; <a href="#type-vclock">vclock()</a>
</code></pre>
<br />

Increment VClock at Node.

<a name="merge-1"></a>

### merge/1 ###

<pre><code>
merge(VClocks::[<a href="#type-vclock">vclock()</a>]) -&gt; <a href="#type-vclock">vclock()</a>
</code></pre>
<br />

Combine all VClocks in the input list into their least possible common descendant.

<a name="prune-3"></a>

### prune/3 ###

<pre><code>
prune(V::<a href="#type-vclock">vclock()</a>, Now::integer(), BucketProps::term()) -&gt; <a href="#type-vclock">vclock()</a>
</code></pre>
<br />

Possibly shrink the size of a vclock, depending on current age and size.

