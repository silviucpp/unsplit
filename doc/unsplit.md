

# Module unsplit #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Framework for merging mnesia tables after netsplit.

__Behaviours:__ [`application`](application.md), [`supervisor`](supervisor.md).

__Authors:__ : Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).

<a name="description"></a>

## Description ##

...

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_reporter-0">get_reporter/0</a></td><td>Look up the predefined callback module for reporting inconsistencies.</td></tr><tr><td valign="top"><a href="#report_inconsistency-4">report_inconsistency/4</a></td><td>Report an inconcistency to the predefined reporter.</td></tr><tr><td valign="top"><a href="#report_inconsistency-5">report_inconsistency/5</a></td><td>Report an inconsistency to Reporter (an unsplit_reporter behaviour).</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Application start callback.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Application stop callback.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_reporter-0"></a>

### get_reporter/0 ###

<pre><code>
get_reporter() -&gt; module()
</code></pre>
<br />

Look up the predefined callback module for reporting inconsistencies

<a name="report_inconsistency-4"></a>

### report_inconsistency/4 ###

<pre><code>
report_inconsistency(Table::any(), Key::any(), ObjectA::any(), ObjectB::any()) -&gt; ok
</code></pre>
<br />

Report an inconcistency to the predefined reporter

<a name="report_inconsistency-5"></a>

### report_inconsistency/5 ###

<pre><code>
report_inconsistency(Reporter::any(), Table::any(), Key::any(), ObjectA::any(), ObjectB::any()) -&gt; ok
</code></pre>
<br />

Report an inconsistency to Reporter (an unsplit_reporter behaviour)

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Type::any(), Arg::[any()]) -&gt; {ok, pid()}
</code></pre>
<br />

Application start callback

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(State::any()) -&gt; ok
</code></pre>
<br />

Application stop callback

