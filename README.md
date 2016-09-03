# Just Fuckin Print RRD

It does exactly what it names. By defalt it prints `DS=value` for all RRD files
given in command line because `collectd` puts all its data to `value`
datasource. It will be little smarter in future, maybe.

## Installation

```shell
git clone git@github.com:s9gf4ult/jfprrd.git
cd jfprrd
stack install
```

now you have `jfprrd` in your `~/.local/bin`

## Examples

```shell
% jfprrd sensors-k10temp-pci-00c3/temperature-temp1.rrd sensors-radeon-pci-0008/temperature-temp1.rrd
```

![xmpl1](https://raw.githubusercontent.com/s9gf4ult/jfprrd/master/doc/img/1.png)
![xmpl2](https://raw.githubusercontent.com/s9gf4ult/jfprrd/master/doc/img/2.png)

Enough.
