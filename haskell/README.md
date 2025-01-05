# Interface to the EurekaPROM in IO mode

This provides a library for easy communication with the
[EurekaPROM](https://www.eurekasound.com/eurekaprom) in IO mode, as well as two
command line applications: one to generate specific MIDI sequences to send to
the device to control the LEDs, and one turn inputs from the device into
programmable keyboard and/or mouse sequences, so that it can be used to control
general applications.

## System requirements

The `eurekaprom-io` library uses
[`alsa-seq`](https://hackage.haskell.org/package/alsa-seq), which requires
requires the ALSA dev system libraries; see https://wiki.haskell.org/ALSA.

The `eurekaprom-io-listen` application additionally uses
[`evdev`](https://hackage.haskell.org/package/evdev), which requires `libevdev`.

On Ubuntu these sytem libraries can be installed using

```
sudo apt install libasound2-dev libevdev-dev
```

For `eurekaprom-io-listen` you will need permission to write to `/dev/uinput`.
See https://github.com/georgefst/evdev/tree/master/evdev#permissions .

## References

ALSA:

* [ALSA documentation](https://www.alsa-project.org/alsa-doc/alsa-lib/seq.html)
* [`alsa-seq` examples](https://archives.haskell.org/code.haskell.org/alsa/seq/examples/)

`libevdev`:

* Blogpost ["The difference between `uinput` and `evdev`"](https://who-t.blogspot.com/2016/05/the-difference-between-uinput-and-evdev.html)
* [Kernel documentation and examples](https://kernel.org/doc/html/v4.12/input/uinput.html)
* [`evdev` examples](https://github.com/georgefst/evdev/tree/master/evdev-examples)
