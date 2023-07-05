

# Module unsplit_lib #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Predefined merge functions.

__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).

<a name="description"></a>

## Description ##

This module implements a few merge functions that can be used.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bag-2">bag/2</a></td><td>Default <code>bag</code> merge; removes duplicate objects.</td></tr><tr><td valign="top"><a href="#last_modified-2">last_modified/2</a></td><td>Keeps the last modified object, based on the <code>modified</code> attribute.</td></tr><tr><td valign="top"><a href="#last_version-2">last_version/2</a></td><td>Picks the object with the greatest value of a given attribute.</td></tr><tr><td valign="top"><a href="#no_action-2">no_action/2</a></td><td>Minimal merge action - does nothing.</td></tr><tr><td valign="top"><a href="#vclock-2">vclock/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bag-2"></a>

### bag/2 ###

<pre><code>
bag(Phase::any(), State::any()) -&gt; <a href="#type-merge_ret">merge_ret()</a>
</code></pre>
<br />

Default `bag` merge; removes duplicate objects

<a name="last_modified-2"></a>

### last_modified/2 ###

<pre><code>
last_modified(Phase::any(), State::any()) -&gt; <a href="#type-merge_ret">merge_ret()</a>
</code></pre>
<br />

Keeps the last modified object, based on the `modified` attribute

This function assumes that the table to be merged contains objects with a `modified` attribute.

<a name="last_version-2"></a>

### last_version/2 ###

<pre><code>
last_version(Phase::any(), State::any()) -&gt; <a href="#type-merge_ret">merge_ret()</a>
</code></pre>
<br />

Picks the object with the greatest value of a given attribute

This function assumes that an attribute name is passed as an extra argument.
e.g. by adding the following user property to the table:

`{unsplit_method, {unsplit_lib, last_version, [Attr]}}`

The function will choose the object that has the greatest value in the
position given by `Attr`.

<a name="no_action-2"></a>

### no_action/2 ###

<pre><code>
no_action(X1::init, State::any()) -&gt; stop
</code></pre>
<br />

Minimal merge action - does nothing

<a name="vclock-2"></a>

### vclock/2 ###

`vclock(Objs, S) -> any()`

